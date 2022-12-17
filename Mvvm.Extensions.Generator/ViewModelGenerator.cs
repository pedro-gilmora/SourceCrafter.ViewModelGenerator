#nullable enable
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Linq;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using System.Reflection;
using System.Collections.Immutable;

namespace Mvvm.Extensions.Generator;

[Generator]
public class ViewModelGenerator : IIncrementalGenerator
{
    static readonly object _lock = new();

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        lock (_lock)
        {
            var interfaceDeclarations = context.SyntaxProvider
                .ForAttributeWithMetadataName(
                    "Mvvm.Extensions.Generator.Attributes.ObservableModelAttribute",
                    static (n, _) => n is InterfaceDeclarationSyntax,
                    static (ctx, c) => new { type = (ITypeSymbol)ctx.TargetSymbol, model = ctx.SemanticModel }
                );

            context.RegisterSourceOutput(
                interfaceDeclarations,
                static (sourceProducer, interfaceToGenerate) =>
                {
                    var (name, code) = CreateRelatedTypeFiles(interfaceToGenerate.type, interfaceToGenerate.model);
                    sourceProducer.AddSource(name, code);
                }
            );
        }
    }

    public static (string, string) CreateRelatedTypeFiles(ITypeSymbol interfaceSymbol, SemanticModel model)
    {
        HashSet<string>
            usings = new() { "Mvvm.Extensions.Generator", "System.ComponentModel", "CommunityToolkit.Mvvm.Input" };
        Dictionary<string, HashSet<string>>
            propertyReferences = new();
        Dictionary<string, List<FieldInfo>>
            typeFieldsHash = new();
        string
            interfaceName = interfaceSymbol.Name,
            name = interfaceName[1..];

        try
        {
            GenerateMembers(model, usings, GetAllMembers(interfaceSymbol), out string fields, out string properties, out string propertyChangeFields, out string commandMethods);

            string classCode = $@"namespace {interfaceSymbol.ContainingNamespace.ToDisplayString()};

public partial class {name} : ViewModelBase, {interfaceName} 
{{
    {new[] { fields, propertyChangeFields, properties, commandMethods }.Where(e => e.Length > 0).Join(@"

    ")}
}}";
            return ($"{name}.g.cs", $@"//<auto generated>
#nullable enable{usings.Join(u => $@"
using {u};")}

{classCode}");

        }
        catch (Exception e)
        {
            return ($"{name}.g.cs", $"/*{e}*/");
        }

    }

    private static void GenerateMembers(SemanticModel model, HashSet<string> usings, IEnumerable<(IPropertySymbol member, bool isImplementedProperty)> enumerable, out string fields, out string properties, out string propertyChangeFields, out string commandMethods)
    {
        fields = properties = propertyChangeFields = commandMethods = "";

        Dictionary<string, HashSet<string>> propReferences = new();
        Dictionary<string, List<(string name, bool isRef)>> fieldsHash = new();

        foreach (var (propSymbol, isImplemented) in enumerable)
        {
            if (properties.Length > 0)
                properties += @"

    ";
            properties += GenerateProperty(usings, propSymbol, isImplemented, model, propReferences, fieldsHash, ref propertyChangeFields, ref commandMethods);
        }

        if (propertyChangeFields.Length > 0)
            propertyChangeFields = $@"private static readonly PropertyChangedEventArgs
        {propertyChangeFields};";

        if (fieldsHash.Count > 0)
            fields = fieldsHash.Select(info => $@"private {info.Key} 
        {info.Value.Join(u => $"{u.name}{(u.isRef ? " = default!" : "")}", @",
        ")};").Join(@"
    ");

    }

    private static IEnumerable<(IPropertySymbol member, bool isImplementedProperty)> GetAllMembers(ITypeSymbol interfaceSymbol)
    {
        return interfaceSymbol
            .GetMembers()
            .Concat(interfaceSymbol.AllInterfaces.SelectMany(iface => iface.GetMembers()))
            .OfType<IPropertySymbol>()
            .Select(m => (m, IsImplementedProperty(m)))
            .OrderByDescending(info => info.Item2);
    }

    private static bool IsImplementedProperty(IPropertySymbol m)
    {
        return m is { IsIndexer: false, GetMethod: var getMethod, SetMethod: var setMethod } && (getMethod?.IsAbstract, setMethod?.IsAbstract) is (false, _) or (_, false);
    }

    private static object GenerateProperty(HashSet<string> usings, IPropertySymbol property, bool isImplementedProperty, SemanticModel model, Dictionary<string, HashSet<string>> propReferences, Dictionary<string, List<(string, bool)>> fields, ref string propertyChangeFields, ref string commandMethods)
    {
        string
            typeName = GetType(usings, property.Type),
            propName = property.Name,
            fieldName = $"_{propName.ToCamelCase()}";

        if (IgnoreProperty(property))
            return $@"public {typeName} {propName} {{{(property.GetMethod == null ? "" : " get;")}{(property.SetMethod == null ? "" : " set;")} }}";

        bool isCommand = typeName.Contains("RelayCommand") && propName.EndsWith("Command");

        ImmutableArray<AttributeData> attributeDatas = property.GetAttributes();

        string? getterBody = null, setterBody = null;

        if (isImplementedProperty)
            CollectDependentProperties(ref getterBody, ref setterBody, ref propertyChangeFields, property.DeclaringSyntaxReferences, property.ContainingType, propReferences, model, propName);
        else
            fields.AddNested(typeName, (fieldName, property.Type.IsReferenceType));
        
        if (isCommand)
            return GenerateCommandProperty(ref commandMethods, attributeDatas, typeName, propName, fieldName);

        UpdatePropertyChangeFields(ref propertyChangeFields, propName, fieldName);

        if (getterBody is ['=', '>', ..])
        {
            return $"public {typeName} {propName} {getterBody};";
        }
        else if (getterBody is { } && setterBody is { })
        {
            return $@"public {typeName} {propName} {{ 
        {getterBody}
        {setterBody}
    }}";
        }

        string
            backingValue = isImplementedProperty
                ? $"(({GetType(usings, property.ContainingType)})this).{propName}"
                : fieldName,
            getter = property.GetMethod == null
                ? ""
                : $@"
        get => {backingValue};",
            setter = property.SetMethod == null
                ? ""
                : $@"
        set {{
            if(value == {backingValue}) 
                return;
            {backingValue} = value;
            OnPropertyChanged({fieldName}ChangedEvtArg);{(propReferences.TryGetValue(propName, out var refs)
                ? refs.Select(p => $@"
            OnPropertyChanged(_{p.ToCamelCase()}ChangedEvtArg);").Join() : "")}
        }}";

        return $@"public {typeName} {propName}{(property is { SetMethod: null, GetMethod.IsAbstract: false } ? $" => {backingValue};" : $@"
    {{{getter}{setter}
    }}")}";

    }

    private static string UpdatePropertyChangeFields(ref string propertyChangeFields, string propName, string fieldName)
    {
        if (propertyChangeFields.Any())
            propertyChangeFields += @",
        ";

        propertyChangeFields += $@"{fieldName}ChangedEvtArg = new(""{propName}"")";
        return propertyChangeFields;
    }

    private static object GenerateCommandProperty(ref string commandMethods, ImmutableArray<AttributeData> attributeDatas, string typeName, string propName, string fieldName)
    {
        var isAsync = typeName.StartsWith("AsyncRelayCommand");
        GetAttrOpts(attributeDatas, out bool checkExec, out string asyncOption);
        string
            baseMethodName = $"Execute{propName[0..^7]}", methodName = $"{baseMethodName}{(isAsync ? "Async" : "")}",
            genericArgumentSyntax = typeName.IndexOf("<") is int a and > 0 ? typeName[a..(typeName.LastIndexOf('>') + 1)] : "",
            genericArgumentName = genericArgumentSyntax is ['<', .. var name, '>'] ? name : "",
            parameterSyntax = genericArgumentName != "" ? $"{genericArgumentName} parameter" : "";

        if (commandMethods.Any())
            commandMethods += @"

    ";
        commandMethods += $@"{(checkExec ? $@"private partial bool Can{baseMethodName}({parameterSyntax});

    " : "")}private partial {(isAsync ? "Task" : "void")} {methodName}({parameterSyntax});";

        return $@"public {typeName} {propName} => {fieldName} ??= new {typeName}({methodName}{(checkExec ? $", Can{baseMethodName}" : "")}{(isAsync ? $", {asyncOption}" : "")});";
    }

    private static void GetAttrOpts(ImmutableArray<AttributeData> attributeDatas, out bool checkExec, out string asyncOption)
    {
        checkExec = true;
        asyncOption = "AsyncRelayCommandOptions.None";
        if (attributeDatas.FirstOrDefault(r => r.AttributeClass?.Name is "CommandOptions" or "CommandOptionsAttribute")?.ApplicationSyntaxReference?.GetSyntax() is AttributeSyntax { ArgumentList.Arguments: [{ Expression: LiteralExpressionSyntax { Token.Value: bool check } }, ..] args })
        {
            checkExec = check;
            if (args.Count == 2)
            {
                asyncOption = args[1].Expression.DescendantNodesAndSelf().OfType<MemberAccessExpressionSyntax>().Select(e => e.ToString()).ToArray().Join(" | ");
            }

        }
    }

    static void UpdatePropertyReferencing<T>(Func<T, IEnumerable<IdentifierNameSyntax>> finder, T expr, SemanticModel model, ITypeSymbol containingType, string propName, Dictionary<string, HashSet<string>> propertyReferences)
        where T : SyntaxNode
    {
        foreach (var identifier in finder(expr))
        {
            if (model.GetSymbolInfo(identifier) is { Symbol: IPropertySymbol prop } &&
                SymbolEqualityComparer.Default.Equals(prop.ContainingType, containingType)
            )
                propertyReferences.AddNested(prop.Name, propName);
        }
    }

    private static void CollectDependentProperties(ref string? getterBody, ref string? setterBody, ref string propertyChangeFields, ImmutableArray<SyntaxReference> references, ITypeSymbol containingType, Dictionary<string, HashSet<string>> propertyReferences, SemanticModel model, string propName)
    {
        var fieldName = $"_{propName.ToCamelCase()}";
        foreach (var declRef in references)

            if (declRef.GetSyntax() is PropertyDeclarationSyntax syntaxReference)
            {
                if (getterBody == null && syntaxReference.ExpressionBody is { })
                {
                    getterBody ??= syntaxReference.ExpressionBody.ToString();
                    UpdatePropertyReferencing(GetReferencedProperties, syntaxReference.ExpressionBody.Expression, model, containingType, propName, propertyReferences);
                }
                else
                    foreach (var accessor in GetPropertyAccessors(syntaxReference))
                        if (accessor is { RawKind: (int)SyntaxKind.GetAccessorDeclaration })
                        {
                            UpdatePropertyReferencing(GetReferencedProperties, accessor, model, containingType, propName, propertyReferences);
                            
                            getterBody ??= accessor.ToString();
                        }
                        else if (accessor is { Modifiers: var mod, Body: var body, ExpressionBody: var exprBody, RawKind: (int)SyntaxKind.SetAccessorDeclaration })
                        {                                
                            UpdatePropertyReferencing(GetAssignedDependentProperties, accessor, model, containingType, propName, propertyReferences);
                            

                            setterBody ??= $@"{mod.Join(" ")} set {{
            {(exprBody is { } ? $"{exprBody};" : body!.Statements.Join(@"
            "))}
            OnPropertyChanged({fieldName}ChangedEvtArg);{(propertyReferences.TryGetValue(propName, out var refs)
                    ? refs.Select(p => $@"
            OnPropertyChanged(_{p.ToCamelCase()}ChangedEvtArg);").Join() : "")}
        }}".Trim();
                        }
            }
    }

    private static IEnumerable<AccessorDeclarationSyntax> GetPropertyAccessors(PropertyDeclarationSyntax syntaxReference)
    {
        return syntaxReference.AccessorList?.Accessors ?? Enumerable.Empty<AccessorDeclarationSyntax>();
    }

    private static IEnumerable<IdentifierNameSyntax> GetAssignedDependentProperties(AccessorDeclarationSyntax syntaxReference)
    {
        return syntaxReference.DescendantNodes().OfType<AssignmentExpressionSyntax>().SelectMany(ass => ass.Left.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>());
    }

    private static IEnumerable<IdentifierNameSyntax> GetReferencedProperties(SyntaxNode syntaxReference)
    {
        return syntaxReference.DescendantNodes().OfType<IdentifierNameSyntax>();
    }

    private static bool IgnoreProperty(IPropertySymbol property)
    {
        return property.GetAttributes().Any(attr => attr.AttributeClass?.Name is "Ignore" or "IgnoreAttribute");
    }

    private static string GetType(HashSet<string> usings, ITypeSymbol type)
    {
        RegisterNamespace(usings, type.ContainingNamespace.ToString());

        return type switch
        {
            INamedTypeSymbol { Name: "Nullable", TypeArguments: [{ } underlyingType] }
                => $"{GetType(usings, underlyingType)}?",

            INamedTypeSymbol { IsTupleType: true, TupleElements: var elements }
                => $"({elements.Join(f => $"{GetType(usings, f.Type)}{(f.IsExplicitlyNamedTupleElement ? $" {f.Name}" : "")}", ", ")})",

            INamedTypeSymbol { Name: var name, TypeArguments: { Length: > 0 } generics }
                => $"{name}<{generics.Join(g => GetType(usings, g), ", ")}>",
            _
                => IsPrimitive((INamedTypeSymbol)type) ? type.ToDisplayString() : type.Name
        };
    }

    private static void RegisterNamespace(HashSet<string> usings, params string[] namespaces)
    {
        foreach (var ns in namespaces)
            if (ns != "<global namespace>")
                usings.Add(ns);
    }

    private static bool IsPrimitive(INamedTypeSymbol type)
    {
        return type?.SpecialType switch
        {
            SpecialType.System_Boolean or
            SpecialType.System_SByte or
            SpecialType.System_Int16 or
            SpecialType.System_Int32 or
            SpecialType.System_Int64 or
            SpecialType.System_Byte or
            SpecialType.System_UInt16 or
            SpecialType.System_UInt32 or
            SpecialType.System_UInt64 or
            SpecialType.System_Decimal or
            SpecialType.System_Single or
            SpecialType.System_Double or
            SpecialType.System_Char or
            SpecialType.System_String => true,
            _ => false
        };
    }
}

internal record struct FieldInfo(string PropName, string FieldName, bool Implemented, bool IsCommand);