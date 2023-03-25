#nullable enable
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections;
using System.Linq;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using System.Collections.Immutable;
using System.Text;

namespace Mvvm.Extensions.Generator;

[Generator]
public class ViewModelGenerator : IIncrementalGenerator
{
    private static readonly object Lock = new();

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        lock (Lock)
        {
            var interfaceDeclarations = context.SyntaxProvider
                .ForAttributeWithMetadataName(
                    "Mvvm.Extensions.Generator.Attributes.ObservableModelAttribute",
                    static (n, _) => n is InterfaceDeclarationSyntax,
                    static (ctx, _) => new { type = (ITypeSymbol)ctx.TargetSymbol, model = ctx.SemanticModel }
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
        HashSet<CSharpSyntaxNode>
            readProps = new();

        string
            interfaceName = interfaceSymbol.Name,
            name = interfaceName[1..];

        try
        {
            StringBuilder propSb = new(),
                propChange = new(),
                fields = new(),
                commandMethods = new(),
                code = new StringBuilder()
                    .AppendFormat(@"

namespace {0};

public partial class {1} : ViewModelBase, {2} {{", interfaceSymbol.ContainingNamespace.ToDisplayString(), name, interfaceName);

            var needsNotifyMethod = false;
                GenerateMembers(propSb, propChange, ref needsNotifyMethod, commandMethods, fields, model, usings, readProps, GetAllMembers(interfaceSymbol));

                code
                    .AppendIf(fields.Length > 0, () => fields.ToString())
                    .AppendIf(propChange.Length > 0, () => propChange.ToString())
                    .AppendIf(propSb.Length > 0, () => propSb.ToString())
                    .AppendIf(commandMethods.Length > 0, () => commandMethods.ToString());
            
            
            code.AppendLine()
                .Append("}")
                .InsertJoin(0, usings, usg => $@"
using {usg};")
                .Insert(0, @"
#nullable enable")
                .Insert(0, @"//<auto generated>");

            return ($"{name}.g.cs", code.ToString());
        }
        catch (Exception e)
        {
            return ($"{name}.g.cs", $"/*{e}*/");
        }
    }
    
    private static void GenerateMembers(StringBuilder propsBuilder, StringBuilder propsChange, ref bool needsNotifyMethod, StringBuilder commandMethods, StringBuilder fieldsSb, SemanticModel model, HashSet<string> usings, HashSet<CSharpSyntaxNode> readProps, IEnumerable<IPropertySymbol> enumerable)
    {
        Dictionary<string, HashSet<string>> propReferences = new();
        Dictionary<string, List<(string name, bool isRef)>> fieldsHash = new();
        

        foreach (var propInfo in enumerable)
        {
            propsBuilder
                    .Append(@"

    ");
            GenerateProperty(propsBuilder, ref needsNotifyMethod, commandMethods,  usings, readProps, propInfo, model, propReferences, fieldsHash);
        }

        if (needsNotifyMethod)
        {
            propsBuilder.Append(@"

    protected void NotifyChange(PropertyChangedEventArgs evtArgs){
        OnPropertyChanged(evtArgs);");

            if (propReferences.Values.SelectMany(e => e.Select(ee => ee)).Any())
            {
                propsBuilder.Append(@"
        switch(evtArgs.PropertyName){");
                propReferences
                    .Aggregate(
                        propsBuilder,
                        (sb, s) =>
                            s.Value.Aggregate(
                                sb.AppendFormat(@"
            case nameof({0}):", s.Key),
                        (_, dep) => _.AppendFormat(@"
                OnPropertyChanged({0});", GetEventName(dep))).Append(@"
                break;")).Append(@"
        }");
            }

            propsBuilder.Append(@" 
    }");
        }
    //
        if (propReferences.Keys
                .Concat(propReferences.Values
                    .SelectMany(e => e.Select(ee => ee)))
                .Distinct()
                .ToList() is { Count: > 0 } propChanges)
        {
            const string propChEvArgsStart = @"

    private static PropertyChangedEventArgs";
            propsChange.Append(propChEvArgsStart);
            propChanges.Aggregate(propsChange, (sb, propName) => sb
                    .AppendIf(sb.Length> propChEvArgsStart.Length, () => ",")
                    .AppendFormat(@"
        {0} = new(nameof({1}))", GetEventName(propName), propName))
                .Append(";");
        }


        if (fieldsHash.Count > 0)
            fieldsHash.Aggregate(
                fieldsSb, 
                (sb, kv) =>
                {
                    return kv.Value.Aggregate((sb: sb.AppendFormat(@"

    private {0}", kv.Key), start: sb.Length), (fSb, info) =>
                    {
                        fSb.sb
                            .AppendIf(fSb.sb.Length > fSb.start, () => ",")
                            .AppendFormat(@"
        {0}", info.name)
                            .AppendIf(info.isRef, () => " = default!");
                        return fSb;
                    }).sb.Append(";");
                });
    }

    private static IEnumerable<IPropertySymbol> GetAllMembers(ITypeSymbol interfaceSymbol)
    {
        return interfaceSymbol
            .GetMembers()
            .Concat(interfaceSymbol.AllInterfaces.SelectMany(iface => iface.GetMembers()))
            .OfType<IPropertySymbol>();
    }

    private static void GenerateProperty(StringBuilder propsSb,
        ref bool needsNotifyMethod,
        StringBuilder commandMethodsSb,
        HashSet<string> usings,
        HashSet<CSharpSyntaxNode> readProps,
        IPropertySymbol propInfo,
        SemanticModel model,
        Dictionary<string, HashSet<string>> propReferences,
        Dictionary<string, List<(string, bool)>> fields)
    {
        var (isImplemented, getter, setter, isReadOnly, ignore) = propInfo; 
        
        string
            typeName = GetType(usings, propInfo.Type),
            propName = propInfo.Name,
            fieldName = GetFieldName(propName);

        if (typeName.Contains("RelayCommand") && propName.EndsWith("Command"))
        {
            fields.AddNested(typeName, (fieldName, true));
            GenerateCommandProperty(propsSb, commandMethodsSb, usings, propInfo.GetAttributes(), typeName, propName, fieldName);
        }
        else
        {
            if (getter is { })
            {
                CollectDependencies(propInfo, propReferences, model, readProps, propInfo);
            }

            propsSb.AppendFormat("public {0} {1} ", typeName, propName);

            if(ignore && (getter,setter) is (null, null))
            {
                propsSb.Append("{");
                if(!propInfo.IsWriteOnly)
                    propsSb.Append(" get;");
                if(!propInfo.IsReadOnly)
                    propsSb.Append(" set;");
                propsSb.Append(" }");
                
                return;
            }


            if (!isReadOnly || ignore)
                propsSb.Append(@"{
        get");

            if (isImplemented)
            {
                propsSb
                    .AppendFormat(" {0};", getter);
            }
            else
            {
                if (!ignore)
                {
                    fields.AddNested(typeName, (fieldName, propInfo.Type.NullableAnnotation != NullableAnnotation.Annotated &&
                        ((propInfo.Type.SpecialType is SpecialType.System_String or SpecialType.System_Object) ||
                        propInfo.Type.TypeKind is TypeKind.Class or TypeKind.TypeParameter || propInfo.Type.IsRecord)));
                    propsSb.AppendFormat(" => {0}", fieldName);
                }
                propsSb.Append(";");
            }

            if (!isReadOnly)
            {
                propsSb.Append(@"
        set");
                if (ignore)
                {
                    propsSb.Append(@"; 
    }");
                    return;
                }
                propsSb.AppendFormat(@" {{
            if(value == {0})
                return;
            ", isImplemented ? propName : fieldName);

                if (!isImplemented)
                {
                    propsSb
                        .AppendFormat("{0} = value;", fieldName);
                    needsNotifyMethod = true;
                    if (!propReferences.ContainsKey(propName))
                        propReferences.Add(propName, new());
                }
                else
                {
                    if (setter is BlockSyntax bs)
                        bs.Statements
                            .Aggregate(propsSb, (sb, st) => sb
                                .AppendFormat("\\n").Append(st));
                    else
                        propsSb
                            .Append(setter);
                }

                propsSb.AppendFormat(@"
            NotifyChange({0});
        }}
    }}", GetEventName(propName));
            }
        }
    }

    private static SymbolEqualityComparer _defaultSymbolComparer = SymbolEqualityComparer.Default;
    private static void CollectDependencies(IPropertySymbol propInfo, Dictionary<string,HashSet<string>> propReferences, SemanticModel model, HashSet<CSharpSyntaxNode> readProps, IPropertySymbol parent)
    {
        if(propInfo.GetMethod?.DeclaringSyntaxReferences
            .LastOrDefault()
            ?.GetSyntax()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>() is { } ids)
        {
            foreach (var item in ids)
            {
                if (model.GetSymbolInfo(item) switch
                    {
                        { Symbol: IPropertySymbol p } when _defaultSymbolComparer.Equals(propInfo.ContainingType, p.ContainingType) => p,
                        { CandidateSymbols: { Length: > 0 } candidates } when
                            candidates
                                .OfType<IPropertySymbol>()
                                .FirstOrDefault(propToCompare =>
                                    _defaultSymbolComparer.Equals(propToCompare.ContainingType, propInfo.ContainingType)) is { } p => p,
                        _ => null

                    } is { } prop)
                {
                    if (!_defaultSymbolComparer.Equals(prop, propInfo) && !prop.IsReadOnly)
                    {
                        propReferences.AddNested(prop.Name, propInfo.Name);
                        propReferences.AddNested(prop.Name, parent.Name);
                    }
                    CollectDependencies(prop, propReferences, model, readProps, parent);
                }
                    
            }
        }
    }

    private static void GenerateCommandProperty(StringBuilder propsSb, StringBuilder commandMethodsSb, HashSet<string> usings,
        ImmutableArray<AttributeData> attributeDatas,
        string typeName, string propName, string fieldName)
    {
        var isAsync = typeName.StartsWith("AsyncRelayCommand");

        if (isAsync)
            RegisterNamespace(usings, "System.Threading.Tasks");

        GetAttrOpts(attributeDatas, out var checkExec, out var asyncOption);
        
        string
            baseMethodName = $"Execute{propName[0..^7]}",
            methodName = $"{baseMethodName}{(isAsync ? "Async" : "")}",
            parameterSyntax = typeName.IndexOf("<", StringComparison.Ordinal) is { } a and > 0 && 
                              typeName[a..(typeName.LastIndexOf('>') + 1)] is ['<', .. { Length: > 0 } genericArgumentName , '>'] ? 
                $"{genericArgumentName} parameter" 
                : "";

        commandMethodsSb.Append(@"

    private partial ");

        if (checkExec)
            commandMethodsSb.AppendFormat(@"bool Can{0}({1});

    private partial", baseMethodName, parameterSyntax);

        commandMethodsSb
            .AppendFormat(@" {0} {1}({2});
    ", isAsync ? "Task" : "void", methodName, parameterSyntax);


        propsSb.AppendFormat("public {0} {1} => {2} ??= new {0}({3}", typeName, propName, fieldName, methodName);

        if (checkExec)
            propsSb.AppendFormat(", Can{0}", baseMethodName);
        
        if (isAsync)
            propsSb.AppendFormat(", {0}", asyncOption);

        propsSb.Append(");");

    }

    private static void GetAttrOpts(ImmutableArray<AttributeData> attributeDatas, out bool checkExec, out string asyncOption)
    {
        checkExec = true;
        asyncOption = "AsyncRelayCommandOptions.None";
        if (attributeDatas.FirstOrDefault("CommandOptionsAttribute".HasName)?.ApplicationSyntaxReference?.GetSyntax() is AttributeSyntax
            {
                ArgumentList.Arguments: [{ Expression: LiteralExpressionSyntax { Token.Value: bool check } }, ..] args
            })
        {
            checkExec = check;
            if (args.Count == 2)
            {
                asyncOption = args[1].Expression.DescendantNodesAndSelf().OfType<MemberAccessExpressionSyntax>().Select(e => e.ToString()).Join(" | ");
            }
        }
    }

    static IEnumerable<IPropertySymbol> UpdatePropertyReferencing<T>(Func<T, IEnumerable<IdentifierNameSyntax>> finder, T expr, SemanticModel model,
        ITypeSymbol containingType, string propName, Dictionary<string, HashSet<string>> propertyReferences)
        where T : SyntaxNode
    {
        foreach (var identifier in finder(expr))
        {
            if (model.GetSymbolInfo(identifier) is { Symbol: IPropertySymbol prop } &&
                SymbolEqualityComparer.Default.Equals(prop.ContainingType, containingType)
               )
                yield return prop;
        }
    }

#pragma warning disable CS0414
    // ReSharper disable once InconsistentNaming
    private static readonly ExpressionSyntax? def = default;
    // ReSharper disable once InconsistentNaming
    private static readonly BlockSyntax? defBS = default;
    // ReSharper disable once InconsistentNaming
    private static readonly AccessorDeclarationSyntax? defAcc = default;
    // ReSharper disable once InconsistentNaming
    private static readonly ArrowExpressionClauseSyntax? defAf = default;
#pragma warning restore CS0414
    private const int 
        GetAccessorDeclaration = (int)SyntaxKind.GetAccessorDeclaration,
        SetAccessorDeclaration = (int)SyntaxKind.SetAccessorDeclaration;

    private static void BuildImplementedProperties(StringBuilder propsSb, ref string? getterBody, ref string? setterBody,
        HashSet<PropertyDeclarationSyntax> readProps,
        ImmutableArray<SyntaxReference> references, ITypeSymbol containingType, Dictionary<string, HashSet<string>> propertyReferences, SemanticModel model,
        string propName)
    {

        foreach (var declRef in references)
        {
            if (declRef?.GetSyntax() is PropertyDeclarationSyntax propSyntax && !readProps.Contains(propSyntax))
            {
                (ArrowExpressionClauseSyntax? exprBody, AccessorDeclarationSyntax? getterAccesor, AccessorDeclarationSyntax? setterAccesor) = propSyntax switch
                {
                    { ExpressionBody: {  } bodyExpr } =>
                        (bodyExpr, defAcc, defAcc),

                    { AccessorList.Accessors:
                        [
                            { RawKind: GetAccessorDeclaration } gAcc
                        ]
                    } =>
                        (defAf, gAcc, defAcc),

                    { AccessorList.Accessors:
                        [
                            { RawKind: GetAccessorDeclaration } gAcc,
                            { RawKind: SetAccessorDeclaration } sAcc
                        ]
                    } =>
                        (defAf, gAcc, sAcc),

                    { AccessorList.Accessors:
                        [
                            { RawKind: SetAccessorDeclaration } sAcc,
                            { RawKind: GetAccessorDeclaration } gAcc
                        ]
                    } =>
                        (defAf, gAcc, sAcc),

                    _ =>
                        (defAf, defAcc, defAcc)
                };

                if (setterAccesor is not null)
                    propsSb.Append("{");

                getterBody = exprBody?.ToString() ?? (getterAccesor switch
                {
                    { Body.Statements: [ReturnStatementSyntax { Expression: { } firstStatement }] } when setterAccesor is null =>
                        SyntaxFactory.ArrowExpressionClause(firstStatement),
                    _ => (CSharpSyntaxNode?)getterAccesor
                })?.ToString();

                if (setterAccesor is { Body.Statements: var body, ExpressionBody.Expression: var expr })
                    setterBody = $@"{{
            {expr?.ToString() ?? body!.Join(@"
            ")}
            OnPropertyChange({GetEventName(propName)});
        }}";

                if (setterAccesor is not null)
                    propsSb.Append("}");
            }
        }
    }

    private static void CollectGetterDependencies(Dictionary<string, HashSet<string>> propDeps,
        ITypeSymbol parent,
        CSharpSyntaxNode getterAccesor,
        bool isReadOnly,
        HashSet<CSharpSyntaxNode> readProps,
        SemanticModel semanticModel,
        string propName)
    {
        if (!readProps.Contains(getterAccesor))
        {
            readProps.Add(getterAccesor);

            foreach (var id in getterAccesor.DescendantNodes().OfType<IdentifierNameSyntax>())
            {
                if (TryGetPropertySymbol(parent, semanticModel, id) is { } prop &&
                    prop.GetMethod?.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax() is CSharpSyntaxNode getter)
                {
                    CollectGetterDependencies(propDeps, parent, getter, prop.IsReadOnly, readProps, semanticModel, prop.Name);
                    
                    propDeps.AddNested(propName, prop.Name);

                    if (!prop.IsReadOnly)
                    {
                        propDeps.AddNested(prop.Name, propName);

                        if (propDeps.TryGetValue(prop.Name, out var deps))
                            foreach (var dep in deps)
                            {
                                if (dep != propName && dep != prop.Name)
                                {
                                    propDeps.AddNested(propName, dep);
                                    propDeps.AddNested(dep, propName);
                                }
                            }
                    }
                }
            }
        }
    }

    private static IPropertySymbol? TryGetPropertySymbol(ITypeSymbol parent, SemanticModel semanticModel, IdentifierNameSyntax id)
    {
        return semanticModel.GetSymbolInfo(id) switch
        {
            { Symbol: IPropertySymbol p } when SymbolEqualityComparer.Default.Equals(parent, p.ContainingType) =>
                p,
            { CandidateSymbols: { Length: > 0 } candidates } when ContainsSameClassProperty(parent, candidates, out var prop) =>
                prop,
            _ => null
        };
    }

    private static bool ContainsSameClassProperty(ITypeSymbol parent, ImmutableArray<ISymbol> candidates, out IPropertySymbol firstOcurrence)
    {
        return (candidates
                .OfType<IPropertySymbol>()
                .FirstOrDefault(p => SymbolEqualityComparer.Default.Equals(parent, p.ContainingType)) switch
            {
                {} pp => (firstOcurrence = pp, true),
                _ => (firstOcurrence = null!, false)
            }).Item2;
    }

    private static string GetFieldName(string propName) => $"_{propName.ToCamelCase()}";

    private static string GetEventName(string valueText) => $"{GetFieldName(valueText)}ChangedEvtArg";

    private static IEnumerable<IdentifierNameSyntax> GetAssignedDependentProperties(AccessorDeclarationSyntax syntaxReference)
    {
        return syntaxReference.DescendantNodes()
            .OfType<AssignmentExpressionSyntax>()
            .SelectMany(ass => ass.Left.DescendantNodesAndSelf()
                .OfType<IdentifierNameSyntax>());
    }

    private static IEnumerable<IdentifierNameSyntax> GetReferencedProperties(SyntaxNode syntaxReference)
    {
        return syntaxReference
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>();
    }

    private static bool IgnoreProperty(IPropertySymbol property)
    {
        return property.GetAttributes().Any(attr => attr.AttributeClass?.Name is "IgnoreAttribute");
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

    public class DependencyNode : HashSet<DependencyNode>
    {
        private DependencyNode(){} 
        public static DependencyNode Empty = new ();
        public DependencyNode(string property, IEnumerable<DependencyNode> dependencies)
        {
            Property = property;
            foreach (var dep in dependencies) Add(dep);
        }

        public string Property { get; }
        
    }
}