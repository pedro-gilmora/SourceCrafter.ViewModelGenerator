using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Mvvm.Extensions.Generator
{
    public sealed class ViewModelGeneratorSyntax
    {
        readonly HashSet<string> usings = new(new[] { "Mvvm.Extensions.Generator", "System.ComponentModel", "CommunityToolkit.Mvvm.Input" });
        readonly HashSet<CSharpSyntaxNode> readProps = new();

        readonly Dictionary<string, Dictionary<string, HashSet<string>>> _propReferences = new();
        readonly Dictionary<string, List<(string name, bool isRef)>> _fields = new();

        private bool _needsNotifyMethod;
        private string _namespace;
        private string _interfaceName;
        public string ClassName { get; }
        private readonly PropertySyntaxInfo[] _properties;

        public readonly string FileName;

        private static readonly SymbolEqualityComparer _defaultSymbolComparer = SymbolEqualityComparer.Default;

        public ViewModelGeneratorSyntax(ITypeSymbol interfaceSymbol, SemanticModel model)
        {
            _namespace = interfaceSymbol.ContainingNamespace.ToDisplayString();
            _interfaceName = interfaceSymbol.Name;
            ClassName = _interfaceName[1..];

            _properties = GetAllMembers(interfaceSymbol)
                .Select(p => GatherPropertyInfo(ref _needsNotifyMethod, usings, p, model, _propReferences, _fields))
                .ToArray();

            FileName = $"{_namespace}.{ClassName}.g.cs";
        }

        private static IEnumerable<IPropertySymbol> GetAllMembers(ITypeSymbol interfaceSymbol)
        {
            return interfaceSymbol
                .GetMembers()
                .Concat(interfaceSymbol.AllInterfaces.SelectMany(iface => iface.GetMembers()))
                .OfType<IPropertySymbol>();
        }

        private static PropertySyntaxInfo GatherPropertyInfo(
            ref bool needsNotifyMethod,
            HashSet<string> usings,
            IPropertySymbol propInfo,
            SemanticModel model,
            Dictionary<string, Dictionary<string, HashSet<string>>> propReferences,
            Dictionary<string, List<(string, bool)>> fields
        )
        {

            string
                typeName = GetType(usings, propInfo.Type),
                propName = propInfo.Name,
                fieldName = GetFieldName(propName);

            var info = PropertySyntaxInfo.Create(propInfo, typeName, propName, fieldName);

            if (info.Getter is { })
            {
                CollectDependencies(propInfo, propReferences, model, propInfo);
            }

            if (info is { IsImplemented: false, Ignore: false })
            {
                if(!info.IsReadOnly || info.IsCommand)
                    fields.AddNested(typeName, (fieldName, ShouldAddInitializer(propInfo)));

                if (!info.IsCommand)
                    needsNotifyMethod = true;
                else if (info.CommandInfo.IsAsync)
                    RegisterNamespace(usings, "System.Threading.Tasks");
            }

            return info;
        }

        private static bool ShouldAddInitializer(IPropertySymbol propInfo) =>
            propInfo.Type.NullableAnnotation != NullableAnnotation.Annotated &&
                ((propInfo.Type.SpecialType is SpecialType.System_String or SpecialType.System_Object) ||
                    propInfo.Type.TypeKind is TypeKind.Class or TypeKind.TypeParameter || propInfo.Type.IsRecord);

        static void BuildProperty(StringBuilder builder, PropertySyntaxInfo info)
        {
            if (info.IsCommand)
            {
                BuildCommandProperty(builder, info.Type, info.Name, info.FieldName, info.CommandInfo);
            }
            else
            {
                builder.AppendFormat(@"
    public {0} {1} ", info.Type, info.Name);

                if (info.Ignore && (info.Getter, info.Setter) is (null, null))
                {
                    builder.Append("{");
                    if (!info.IsWriteOnly)
                        builder.Append(" get;");
                    if (!info.IsReadOnly)
                        builder.Append(" set;");
                    builder.Append(" }");

                    return;
                }


                if (!info.IsReadOnly || info.Ignore)
                    builder.Append(@"{
        get");

                if (info.IsImplemented)
                {
                    builder
                        .AppendFormat(" {0};", info.Getter);
                }
                else
                {
                    if (!info.Ignore)
                    {
                        builder.AppendFormat(" => {0}", info.FieldName);
                    }
                    builder.Append(";");
                }

                if (!info.IsReadOnly)
                {
                    builder.Append(@"
        set");
                    if (info.Ignore)
                    {
                        builder.Append(@"; 
    }");
                        return;
                    }
                    builder.AppendFormat(@" {{
            if(value == {0})
                return;
            ", info.IsImplemented ? info.Name : info.FieldName);

                    if (!info.IsImplemented)
                    {
                        builder
                            .AppendFormat("{0} = value;", info.FieldName);
                    }
                    else
                    {
                        if (info.Setter is BlockSyntax bs)
                            bs.Statements
                                .Aggregate(builder, (sb, st) => sb
                                    .AppendFormat(@"
").Append(st));
                        else
                            builder
                                .Append(info.Setter);
                    }

                    builder.AppendFormat(@"
            NotifyChange(new(nameof({0})));
        }}
    }}", info.Name);
                }
            }
        }

        private static void BuildCommandProperty(StringBuilder builder, string typeName, string propName, string fieldName, CommandInfo info)
        {
            string
                baseMethodName = $"Execute{propName[0..^7]}",
                methodName = $"{baseMethodName}{(info.IsAsync ? "Async" : "")}",
                parameterSyntax = typeName.IndexOf("<", StringComparison.Ordinal) is { } a and > 0 &&
                                  typeName[a..(typeName.LastIndexOf('>') + 1)] is ['<', .. { Length: > 0 } genericArgumentName, '>'] ?
                    $"{genericArgumentName} parameter"
                    : "";

            builder.AppendFormat(@"
    public {0} {1} => {2} ??= new {0}({3}", typeName, propName, fieldName, methodName);

            if (info.CheckExec)
                builder.AppendFormat(", Can{0}", baseMethodName);

            if (info.IsAsync)
                builder.AppendFormat(", {0}", info.AsyncOption);

            builder.Append(@");

    private partial ");

            if (info.CheckExec)
                builder.AppendFormat(@"bool Can{0}({1});

    private partial", baseMethodName, parameterSyntax);

            builder
                .AppendFormat(@" {0} {1}({2});
    ", info.IsAsync ? "Task" : "void", methodName, parameterSyntax);

        }

        private static void CollectDependencies(IPropertySymbol propInfo, Dictionary<string, Dictionary<string, HashSet<string>>> propReferences, SemanticModel model, IPropertySymbol parent)
        {

            if (propInfo.GetMethod?.DeclaringSyntaxReferences
                .LastOrDefault()
                ?.GetSyntax()
                .DescendantNodes()
                .OfType<IdentifierNameSyntax>() is { } ids) // Get all identifiers in the expression tree from the getter accessor 
            {
                MemberAccessExpressionSyntax? lastAccessExpr = default;

                foreach (var item in ids)
                {
                    if (item.Parent is MemberAccessExpressionSyntax { Span.End: var end } memberAccessExpr && end != item.Span.End) // it's a parentPropName MemberAccessExpressionSyntax for the current identifier 
                    {
                        lastAccessExpr ??= memberAccessExpr;
                        continue; //Skip to last Identifier in the MemberAccessExpressionSyntax
                    }
                    else if(item.Parent is IsPatternExpressionSyntax)

                    if (model.GetSymbolInfo(item) switch
                    {
                        { Symbol: IPropertySymbol p } => p,

                        { CandidateSymbols: [IPropertySymbol { } p] candidates } => p,
                        _ => null

                    } is { } prop) //If it found a symbol
                    {
                        string propName = string.Intern(prop.Name);
                        bool isSameType = _defaultSymbolComparer.Equals(prop.ContainingType, parent.ContainingType);
                        if (
                            item.Parent == lastAccessExpr && //The parentPropName is a MemberAccessExpressionSyntax too
                            model.GetSymbolInfo(lastAccessExpr!.Expression) is { Symbol: IPropertySymbol { Type: { } prnt } } && // its owning symbols is a IPropertySymbol
                            prnt.ContainsAttribute("Mvvm.Extensions.Generator.Attributes", "ObservableModel") && // it's an observable model
                            !isSameType // it's from a different type
                        )
                        {
                            UpdateReferences(propReferences, lastAccessExpr.Expression.ToString(), parent.Name, "$" + lastAccessExpr.Name);
                            lastAccessExpr = null;
                        }
                        else if (!prop.ContainsAttribute("Ignore")) // the property should not be marked as ignore
                        {
                            if (lastAccessExpr != null && 
                                model.GetSymbolInfo(lastAccessExpr.Expression) is { Symbol: IPropertySymbol { ContainingType: { } type } maep } &&
                                _defaultSymbolComparer.Equals(type, propInfo.ContainingType)) 
                            {
                                UpdateReferences(propReferences, maep.Name, propInfo.Name);
                            }
                            else if (!_defaultSymbolComparer.Equals(prop, propInfo) && !prop.IsReadOnly)// are not the same type nor readonly
                            {
                                UpdateReferences(propReferences, propName, propInfo.Name);
                                UpdateReferences(propReferences, propName, parent.Name);
                            }
                            CollectDependencies(prop, propReferences, model, parent);
                        }
                    }

                }
            }
        }

        private static void UpdateReferences(Dictionary<string, Dictionary<string, HashSet<string>>> propReferences, string key1, string parentPropName, string? lastAccessExpr = null)
        {
            HashSet<string> value = new() { };

            if (lastAccessExpr != null)
                value.Add(parentPropName);

            if (propReferences.TryGetValue(key1, out var hash))
            {
                if (!hash.TryGetValue(parentPropName, out var subHash))
                    hash[parentPropName] = value;
                if (lastAccessExpr != null && hash.TryGetValue(lastAccessExpr, out subHash))
                    subHash.Add(parentPropName);
            }
            else
                propReferences[key1] = lastAccessExpr != null
                    ? new() { { lastAccessExpr, value } }
                    : new() { { parentPropName, value } };
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

        private static string GetFieldName(string propName) => $"_{propName.ToCamelCase()}";

        public override string ToString()
        {
            StringBuilder code = new StringBuilder().Append(@"//<auto generated>
#nullable enable");
            //Generate usings
            usings.Aggregate(code, (sb, us) => sb.AppendFormat(@"
using {0};", us))

            .AppendFormat(@"

namespace {0};

public partial class {1} : ViewModelBase, {2} {{", _namespace, ClassName, _interfaceName);

            int initLen = code.Length;

            //Build fields syntax
            _fields.Aggregate(code, (_, kv) =>
            {
                var start = code.AppendFormat(@"

    private {0}", kv.Key);

                initLen = code.Length;

                kv.Value.Aggregate(start, (_, fieldInfo) =>
                {
                    if (code.Length > initLen)
                        code.Append(",");

                    code.AppendFormat(@"
        {0}", fieldInfo.name);

                    if (fieldInfo.isRef)
                        code.Append(" = default!");

                    return code;
                });

                return start.Append(";");
            });

            initLen = code.Length;

            //Build properies
            foreach (var pInfo in _properties)
            {
                code.Append(@"
    ");

                BuildProperty(code, pInfo);
            }



            if (_needsNotifyMethod)
            {
                code.Append(@"

    protected void NotifyChange(PropertyChangedEventArgs evtArgs) {
        OnPropertyChanged(evtArgs);");

                if (_propReferences.Values.Any(v => v.Any()))
                {
                    code.Append(@"
        switch(evtArgs.PropertyName){");
                    _propReferences
                        .Aggregate(
                            code,
                            (_, pInfo) =>
                            {

                                code.AppendFormat(@"
            case nameof({0}):", pInfo.Key);

                                var openSubscription = 0;

                                foreach (var item in pInfo.Value.OrderByDescending(v => v.Key.StartsWith("$")))
                                {
                                    if (item.Key.StartsWith("$"))
                                    {
                                        if (openSubscription == 0)
                                        {
                                            openSubscription = 1;
                                            code.AppendFormat(@"
                ({0} as IObservable)?.Subscribe((s, a) => 
                {{
                    switch(a.PropertyName){{", pInfo.Key);
                                        }

                                        code.AppendFormat(@"
                        case ""{0}"":", item.Key[1..]);

                                        foreach (var item2 in item.Value)
                                        {
                                            code.AppendFormat(@"
                            OnPropertyChanged(new(nameof({0})));", item2);

                                        }
                                        code.Append(@"
                            break;");
                                    }
                                    else
                                    {
                                        if (openSubscription == 1)
                                        {
                                            openSubscription = 0;
                                            code.Append(@"
                    }
                });");
                                        }
                                        code.AppendFormat(@"
                OnPropertyChanged(new(nameof({0})));", item.Key);
                                    }


                                }
                                if (openSubscription == 1)
                                {
                                    openSubscription = 0;
                                    code.Append(@"
                    }
                });");
                                }

                                return code.Append(@"
                break;");
                            }).Append(@"
        }");
                }

                code.Append(@"
    }");
            }

            code.Append(@"
}");
            return code.ToString();

        }

        internal record struct PropertySyntaxInfo(string Type, string Name, string FieldName, bool IsImplemented, CSharpSyntaxNode? Getter, CSharpSyntaxNode? Setter, bool IsReadOnly, bool IsWriteOnly, bool Ignore, bool IsCommand, CommandInfo CommandInfo)
        {
            public static PropertySyntaxInfo Create(IPropertySymbol propSymbol, string typeName, string propName, string fieldName)
            {
                var isCommand = propSymbol.Type.Name.Contains("RelayCommand") && propSymbol.Name.EndsWith("Command");
                var isAsyncCommand = isCommand && propSymbol.Type.Name.StartsWith("Async");

                var (isImplemented, isReadOnly, isWriteOnly, getter, setter, ignore) = propSymbol switch
                {
                    { IsIndexer: false, DeclaringSyntaxReferences: [.., { } propSyntaxRef] } when propSyntaxRef.GetSyntax() is PropertyDeclarationSyntax { ExpressionBody: { } body } =>
                        (true, true, false, body, null, propSymbol.ContainsAttribute("Ignore")),
                    { IsIndexer: false, GetMethod: var getMethod, SetMethod: var setMethod, IsReadOnly: var isRo, IsWriteOnly: var isWo } =>
                        (!(getMethod?.IsAbstract ?? true),
                            isRo,
                            isWo,
                            TryReduceMethod(getMethod),
                            TryReduceMethod(setMethod),
                            propSymbol.ContainsAttribute("Ignore")),
                    _ => default
                };
                bool checkExec = default;
                string? asyncOption = default;
                if (isCommand)
                    GetCommandOptions(propSymbol.GetAttributes(), out checkExec, out asyncOption);
                return new(typeName, propName, fieldName, isImplemented, getter, setter, isReadOnly, isWriteOnly, ignore, isCommand, new(isAsyncCommand, checkExec, asyncOption));
            }

            private const int
                SetAccessorDeclaration = (int)SyntaxKind.SetAccessorDeclaration;

            private static CSharpSyntaxNode? TryReduceMethod(IMethodSymbol? method) => (method?.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax() as AccessorDeclarationSyntax) switch
            {
                { RawKind: { } kind, ExpressionBody: { } retValue } =>
                    kind == SetAccessorDeclaration
                        ? ExpressionStatement(retValue.Expression)
                        : retValue,
                { RawKind: { } kind, Body.Statements: [{ } retValue] } =>
                    retValue,
                var ret =>
                    ret?.Body
            };

            private static void GetCommandOptions(ImmutableArray<AttributeData> attributeDatas, out bool checkExec, out string asyncOption)
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
                        asyncOption = args[1].Expression.DescendantNodesAndSelf().OfType<MemberAccessExpressionSyntax>().Join(" | ");
                    }
                }
            }
        }

        internal record struct CommandInfo(bool IsAsync, bool CheckExec, string? AsyncOption);
    }
}
