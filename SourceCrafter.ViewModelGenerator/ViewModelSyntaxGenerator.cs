using System.Reflection;
using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("SourceCrafter.ViewModelGenerator.UnitTests")]

namespace SourceCrafter
{
    internal sealed class ViewModelSyntaxGenerator
    {
        readonly HashSet<string> usings = new(new[] { "SourceCrafter.Mvvm", "System.ComponentModel", "CommunityToolkit.Mvvm.Input" });
        //readonly HashSet<CSharpSyntaxNode> readProps = new();

        readonly PropertyDependencyTree dependencies = new();
        readonly Dictionary<string, List<(string name, bool isRef)>> _fields = new();

        private bool _needsNotifyMethod;
        private readonly string _namespace;
        private readonly string _interfaceName;
        public string ClassName { get; }
        private readonly ImmutableArray<PropertySyntaxInfo> _properties;

        public readonly string FileName;

        //private static readonly Func<ISymbol?, ISymbol?, bool> AreSymbolsEquals = SymbolEqualityComparer.Default.Equals;
        internal const string NAMESPACE = "SourceCrafter.Mvvm.Attributes";
        internal const string ATTRIBUTE = "ObservableModelAttribute";

        public ViewModelSyntaxGenerator(ITypeSymbol interfaceSymbol, SemanticModel model)
        {
            _namespace = interfaceSymbol.ContainingNamespace.ToDisplayString();
            _interfaceName = interfaceSymbol.Name;
            ClassName = _interfaceName[1..];

            _properties = GetAllMembers(interfaceSymbol)
                .Select(p => GatherPropertyInfo(ref _needsNotifyMethod, usings, p, model, dependencies, _fields))
                .ToImmutableArray();

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
            PropertyDependencyTree propReferences,
            Dictionary<string, List<(string, bool)>> fields
        )
        {
            string
                typeName = GetType(usings, propInfo.Type),
                propName = propInfo.Name,
                fieldName = GetFieldName(propName);

            var info = PropertySyntaxInfo.Create(propInfo, typeName, propName, fieldName);

            if (!info.Ignore && info.Getter != null)
            {
                CollectDependencies(info.Getter, propReferences, model, propName);
            }

            if (info is { IsImplemented: false, Ignore: false })
            {
                if (!info.IsReadOnly || info.IsCommand)
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

        private static void CollectDependencies(SyntaxNode node, PropertyDependencyTree dependencies, SemanticModel model, string propName)
        {
            var subNodes = node.DescendantNodes().OfType<ExpressionSyntax>().ToImmutableArray().GetEnumerator();
            var pos = node.Span.Start;

            if (node is not ExpressionSyntax)
                goto findSubNodes;
            parse:;

            if (node is InterpolatedStringExpressionSyntax interpolationExpr)
            {
                foreach (var interpolation in interpolationExpr.Contents.OfType<InterpolationSyntax>())
                {
                    if (interpolation.Span.Start < pos)
                        continue;
                    pos = interpolation.Span.End;
                    CollectDependencies(interpolation.ChildNodes().First(), dependencies, model, propName);
                }
            }
            else if (node is IsPatternExpressionSyntax isPattern)
            {
                pos = node.Span.End;
                var exprProps = GetIdentifiers(isPattern.Expression, model).ToImmutableArray();
                var subPatterns = ExtractDependantTree(isPattern, exprProps, model).ToImmutableArray();
                if (subPatterns.Length > 0)
                    foreach (var ids in subPatterns)
                    {

                        var lastWasReactive = true;
                        var deps = dependencies;

                        foreach (var id in ids)
                            if (lastWasReactive)
                            {
                                deps = deps.AddDependencies(true, id);
                                deps.NotifyTo.Add(propName);
                                lastWasReactive = IsNotifiable(id);
                            }
                    }
                else if (exprProps.Length > 0)
                {
                    var lastWasReactive = true;
                    var deps = dependencies;

                    foreach (var id in exprProps)
                        if (lastWasReactive)
                        {
                            deps = deps.AddDependencies(true, id);
                            deps.NotifyTo.Add(propName);
                            lastWasReactive = IsNotifiable(id);
                        }
                }
            }
            else if (node is ConditionalAccessExpressionSyntax or MemberAccessExpressionSyntax)
            {
                pos = node.Span.End;

                var lastWasReactive = true;
                var deps = dependencies;

                foreach (var id in GetIdentifiers(node, model))
                    if (lastWasReactive)
                    {
                        deps = deps.AddDependencies(true, id);
                        deps.NotifyTo.Add(propName);
                        lastWasReactive = IsNotifiable(id);
                    }
            }
            else if (node is IdentifierNameSyntax id &&
                model.GetSymbolInfo(id).Symbol is IPropertySymbol p)
            {
                dependencies.AddDependencies(true, p).NotifyTo.Add(propName);
            }

        findSubNodes: while (subNodes.MoveNext())
                if (pos >= (node = subNodes.Current).Span.End || node.Span.Start < pos || node.Parent is InvocationExpressionSyntax)
                    continue;
                else
                    goto parse;
        }

        private static bool IsNotifiable(IPropertySymbol id)
        {
            return id.Type.HasAttribute(NAMESPACE, ATTRIBUTE);
        }

        static IEnumerable<IEnumerable<IPropertySymbol>> ExtractDependantTree(IsPatternExpressionSyntax isPattern, IEnumerable<IPropertySymbol> exprIds, SemanticModel model)
        {
            return isPattern.Pattern
                .DescendantNodes()
                .OfType<SubpatternSyntax>()
                .Where(el => el.Pattern is not RecursivePatternSyntax)
                .Select(subPattern =>
                    exprIds
                    .Concat(EnumerateParentPatterns(subPattern.ExpressionColon!, isPattern.Pattern, model)));
        }

        static IEnumerable<IPropertySymbol> EnumerateParentPatterns(BaseExpressionColonSyntax exprCol, PatternSyntax topPattern, SemanticModel model)
        {
            return exprCol
                .Ancestors()
                .TakeWhile(ec => ec != topPattern)
                .Where(eca => eca is SubpatternSyntax pattern && pattern != exprCol.Parent)
                .SelectMany(eca => GetIdentifiers(((SubpatternSyntax)eca).ExpressionColon!, model))
                .Reverse()
                .Concat(GetIdentifiers(exprCol, model));
        }

        static IEnumerable<IPropertySymbol> GetIdentifiers(SyntaxNode expressionColon, SemanticModel model)
        {
            foreach (var item in expressionColon.DescendantNodesAndSelf())
                if (item is IdentifierNameSyntax && model.GetSymbolInfo(item).Symbol is IPropertySymbol prop)
                    yield return prop;
        }

        private static string GetType(HashSet<string> usings, ITypeSymbol type)
        {
            RegisterNamespace(usings, type.ContainingNamespace.ToString());

            string typeName = type switch
            {
                IArrayTypeSymbol { ElementType: { } arrayType }
                => GetType(usings, arrayType) + "[]",

                INamedTypeSymbol { IsTupleType: true, TupleElements: var elements }
                    => $"({elements.Join(f => f.IsExplicitlyNamedTupleElement ? GetType(usings, f.Type) + " " + f.Name : GetType(usings, f.Type), ", ")})",

                INamedTypeSymbol { Name: "Nullable", TypeArguments: [{ } underlyingType] } =>
                        GetTypeName(usings, underlyingType, true),

                INamedTypeSymbol { Name: var name, TypeArguments: { Length: > 0 } generics }
                        => $"{name}<{generics.Join(g => GetType(usings, g), ", ")}>",
                _
                    => GetTypeName(usings, type)
            };

            return typeName;
        }

        private static string GetTypeName(HashSet<string> usings, ITypeSymbol type, bool addNullability = false)
        {
            bool isPrimitive = IsPrimitive((INamedTypeSymbol)type);
            var typeName = isPrimitive ? type.ToDisplayString() : type.Name;

            if (!isPrimitive && type.ContainingNamespace!=null)
                RegisterNamespace(usings, type.ContainingNamespace.ToString());

            if (typeName[^1] != '?' && (addNullability || type is { NullableAnnotation: NullableAnnotation.Annotated }))
                typeName += '?';

            return typeName;
        }

        private static void RegisterNamespace(HashSet<string> usings, params string[] namespaces)
        {
            foreach (var ns in namespaces)
                if (ns != "<global namespace>")
                    usings.Add(ns);
        }

        private static bool IsPrimitive(INamedTypeSymbol type)
        {
            return type?.SpecialType is
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
                SpecialType.System_String;
        }

        private static string GetFieldName(string propName) => $"_{propName.ToCamelCase()}";

        public override string ToString()
        {
            var code = new StringBuilder().Append(@"//<auto generated>
#nullable enable");
            //Generate usings
            usings.Aggregate(code, (sb, us) => sb.AppendFormat(@"
using {0};", us))

            .AppendFormat(@"

namespace {0};

public partial class {1} : ViewModelBase, {2} {{", _namespace, ClassName, _interfaceName);

            BuildFields(code);

            BuildProperties(code);

            if (_needsNotifyMethod)
            {
                code.Append(@"

    protected void NotifyChange(PropertyChangedEventArgs evtArgs) {
        OnPropertyChanged(evtArgs);");

                BuildSwitch(code, dependencies);

                code.Append(@"
    }");
            }

            code.Append(@"
}");
            return code.ToString();

        }

        private void BuildFields(StringBuilder code)
        {
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
        }

        void BuildProperties(StringBuilder code)
        {
            //Build properies
            foreach (var prop in _properties)
            {
                code.Append(@"
    ");
                var hasDependencies = dependencies.TryGetValue(prop.Symbol, out var deps) && deps.NotifyTo.Any();

                if (prop.IsCommand)
                {
                    BuildCommandProperty(code, prop.Type, prop.Name, prop.BackingField, prop.CommandInfo);
                }
                else
                {
                    code.AppendFormat(@"
    public {0} {1}", prop.Type, prop.Name);

                    if (prop.IsReadOnly && (prop.IsSingleStatementGetter))
                    {
                        code.Append($" => {prop.Getter};");
                        continue;
                    }

                    code.Append(" {");

                    if (!prop.IsWriteOnly)
                    {
                        code.Append($@"{(prop.IsReadOnly || prop.Ignore ? " " : @"
        ")}get");

                        if (prop.UseBackingField || prop.IsSingleStatementGetter)
                            code.Append(" =>");

                        code.Append($" {(prop.UseBackingField ? prop.BackingField : prop.Getter)}".TrimEnd());

                        if (!prop.IsImplemented || prop.IsSingleStatementGetter)
                            code.Append(";");


                        if (prop.IsReadOnly)
                            continue;
                    }

                    if (!prop.IsReadOnly)
                    {
                        code.Append(prop.IsWriteOnly || prop.Ignore ? " " : @"
        ");
                        code.Append("set");

                        if (prop.Ignore)
                        {
                            if (prop.IsSingleStatementSetter)
                                code.Append(" =>");

                            code.Append($" {prop.Setter}".TrimEnd());

                            if (prop.IsSingleStatementSetter || prop.Getter == null)
                                code.Append(";");

                            if (!prop.IsImplemented)
                            {
                                code.Append(" }");
                                continue;
                            }
                        }
                        else
                        {
                            code.Append($@" {{
            if(Equals(value, {(prop.UseBackingField ? prop.BackingField : prop.Name)}))
                return;
            ");

                            if (prop.UseBackingField && !prop.IsWriteOnly)
                                code.Append($@"{prop.BackingField} = value;");

                            if (!prop.IsSingleStatementSetter && prop.Setter is BlockSyntax bs)
                                bs.Statements
                                    .Aggregate(code, (sb, st) => sb
                                        .Append($@"
            {st}"));
                            else if (prop.Setter != null)
                                code.Append(prop.Setter)
                                    .Append(";");

                            code.Append($@"
            {(hasDependencies ? "NotifyChange" : "OnPropertyChanged")}(new(""{prop.Name}""));");

                            code.Append(@"
        }");
                        }

                    }

                    code.Append(@"
    }");

                }
            }
        }

        private static void BuildCommandProperty(StringBuilder builder, string typeName, string propName, string fieldName, CommandInfo info)
        {
            string
                baseMethodName = $"Execute{propName[0..^7]}",
                methodName = info.IsAsync ? $"{baseMethodName}Async" : baseMethodName,
                parameterSyntax = typeName.IndexOf("<", StringComparison.Ordinal) is { } a and > 0 &&
                                  typeName[a..(typeName.LastIndexOf('>') + 1)] is ['<', .. { Length: > 0 } genericArgumentName, '>']
                                  ? $"{genericArgumentName} parameter"
                                  : "";

            builder.AppendFormat(@"
    public {0} {1} => {2} ??= new {0}({3}", typeName, propName, fieldName, methodName);

            if (info.CheckExec)
                builder.AppendFormat(", Can{0}", baseMethodName);

            if (info.IsAsync)
                builder.AppendFormat(", {0}", info.AsyncOption);

            builder.Append(@");

    ");

            if (info.CheckExec)
                builder.AppendFormat(@"private partial bool Can{0}({1});

    ", baseMethodName, parameterSyntax);

            builder
                .AppendFormat(@"{0} {1}({2});
    ", info.IsAsync ? "private partial Task" : "partial void", methodName, parameterSyntax);

        }

        private void BuildSwitch(StringBuilder code, PropertyDependencyTree dependencies, int indent = 2, string parentEvtArgName = "evtArgs", int level = 0)
        {
            if (dependencies.Count == 0) return;

            string indentStr = new(' ', indent * 4);

            code.Append($@"
{indentStr}switch({parentEvtArgName}.PropertyName){{");
            foreach (var (prop, deps) in dependencies)
            {
                code.Append($@"
{indentStr}    case ""{prop.Name}"":");

                if (deps.Count > 0)
                {
                    code.Append($@"
{indentStr}        ({prop.Name} as IObservable)?.Subscribe((s{level}, e{level}) => {{");
                    BuildSwitch(code, deps, indent + 3, $"e{level}", level + 1);
                    code.Append($@"
{indentStr}        }});");
                }

                foreach (var item in deps.NotifyTo)
                {
                    code.Append($@"
{indentStr}        OnPropertyChanged(new(""{item}""));");
                }

                code.Append($@"
{indentStr}    break;");
            }

            code.Append($@"
{indentStr}}}");
        }

        internal record struct PropertySyntaxInfo(string Type, string Name, string BackingField, bool IsImplemented, CSharpSyntaxNode? Getter, CSharpSyntaxNode? Setter, bool IsReadOnly, bool IsWriteOnly, bool Ignore, bool IsCommand, CommandInfo CommandInfo)
        {
            public static PropertySyntaxInfo Create(IPropertySymbol propSymbol, string typeName, string propName, string fieldName)
            {
                var isCommand = propSymbol.Type.Name.Contains("RelayCommand") && propSymbol.Name.EndsWith("Command");
                var isAsyncCommand = isCommand && propSymbol.Type.Name.StartsWith("Async");

                var (isImplemented, isReadOnly, isWriteOnly, getter, setter, ignore) = propSymbol switch
                {
                    { IsIndexer: false, DeclaringSyntaxReferences: [.., { } propSyntaxRef] } when propSyntaxRef.GetSyntax() is PropertyDeclarationSyntax { ExpressionBody.Expression: { } body } =>
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
                return new(typeName, propName, fieldName, isImplemented, getter, setter, isReadOnly, isWriteOnly, ignore, isCommand, new(isAsyncCommand, checkExec, asyncOption))
                {
                    Symbol = propSymbol,
                    IsSingleStatementGetter = getter is { } and not BlockSyntax,
                    IsSingleStatementSetter = setter is { } and not BlockSyntax
                };
            }

            private const int SetAccessorDeclaration = (int)SyntaxKind.SetAccessorDeclaration,
                GetAccessorDeclaration = (int)SyntaxKind.GetAccessorDeclaration;

            public bool UseBackingField => !Ignore && !IsImplemented && !IsReadOnly;

            public IPropertySymbol Symbol { get; internal set; }
            public bool IsSingleStatementGetter { get; private set; }
            public bool IsSingleStatementSetter { get; private set; }

            private static CSharpSyntaxNode? TryReduceMethod(IMethodSymbol? method) =>
                (method?.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax() as AccessorDeclarationSyntax) switch
                {
                    { RawKind: { } kind, ExpressionBody: { Expression: { } expr } } =>
                        expr,
                    { RawKind: { } kind, Body.Statements: [{ } retValue] } =>
                        (CSharpSyntaxNode)retValue.ChildNodes().First(),
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

        internal class PropertyDependencyTree : Dictionary<IPropertySymbol, PropertyDependencyTree>
        {
            public HashSet<string> NotifyTo = new();
            public PropertyDependencyTree AddDependencies(bool returnDeep, params IPropertySymbol[] children)
            {
                return AddDependencies(returnDeep, (IEnumerable<IPropertySymbol>)children);
            }

            public PropertyDependencyTree AddDependencies(bool returnDeep, IEnumerable<IPropertySymbol> children)
            {
                var currentDependency = this;

                foreach (var segment in children)
                {
                    if (!currentDependency.TryGetValue(segment, out var childDependency))
                    {
                        childDependency = currentDependency[segment] = new();
                    }
                    currentDependency = childDependency;
                }

                return returnDeep ? currentDependency : this;
            }
        }

    }
}
