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
using System.Runtime.InteropServices;
using System.Buffers;
using System.Diagnostics;
using System.Security.Cryptography;
using System.Xml.Linq;
using System.Linq.Expressions;
using Microsoft.CodeAnalysis.Text;
using System.ComponentModel;
using static SourceCrafter.ViewModelSyntaxGenerator;
using HttPie.Generator.Constants;

[assembly: InternalsVisibleTo("SourceCrafter.ViewModelGenerator.UnitTests")]

namespace SourceCrafter;

internal sealed class ViewModelSyntaxGenerator
{
    internal static SymbolDisplayFormat GlobalizedNamespace = new(
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters | SymbolDisplayGenericsOptions.IncludeVariance,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes | SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier);

    readonly PropertyDependencyTree _dependencies;
    readonly Dictionary<string, List<(string name, bool isNullable, bool allowsNull)>> _fields = new();
    readonly string _namespace;
    readonly string _interfaceName;
    readonly ImmutableArray<PropertySyntaxInfo> _properties;

    bool _needsNotifyMethod;
    public string ClassName { get; }

    public readonly string FileName;
    private readonly SemanticModel _model;
    private readonly StringBuilder _builder;

    //private static readonly Func<ISymbol?, ISymbol?, bool> AreSymbolsEquals = SymbolEqualityComparer.Default.Equals;
    internal const string NAMESPACE = "SourceCrafter.Mvvm.Attributes";
    internal const string ATTRIBUTE = "Reactive";

    public ViewModelSyntaxGenerator(ITypeSymbol interfaceSymbol, SemanticModel model)
    {
        _model = model;
        _namespace = interfaceSymbol.ContainingNamespace.ToDisplayString(GlobalizedNamespace);
        _interfaceName = interfaceSymbol.Name;
        _dependencies = new(model);

        ClassName = _interfaceName[1..];

        _properties = GetAllMembers(interfaceSymbol)
            .Select(CollectPropertyInfo)
            .ToImmutableArray();

        FileName = string.Format("{0}.{1}.g.cs", interfaceSymbol.ContainingNamespace.ToString().Replace("<global namespace>", ""), ClassName);
        _builder = new StringBuilder();
    }

    private static ImmutableArray<IPropertySymbol> GetAllMembers(ITypeSymbol interfaceSymbol)
    {
        var arr = ImmutableArray.CreateBuilder<IPropertySymbol>();

        foreach (var item in interfaceSymbol.GetMembers())
        {
            if (item is not IPropertySymbol prop) continue;
            arr.Add(prop);
        }

        foreach (var iface in interfaceSymbol.AllInterfaces)
        {
            foreach (var item in iface.GetMembers())
            {
                if (item is not IPropertySymbol prop2) continue;
                arr.Add(prop2);
            }
        }

        return arr.ToImmutableArray();
    }

    private PropertySyntaxInfo CollectPropertyInfo(IPropertySymbol propInfo)
    {
        string
            propName = propInfo.Name,
            typeName = propInfo.Type.ToDisplayString(GlobalizedNamespace),
            fieldName = GetFieldName(propName);
        
        var info = PropertySyntaxInfo.Create(propInfo, typeName, propName, fieldName);

        if (!info.Ignore && info.Getter != null)
        {
            Dictionary<SyntaxNode, HashSet<PropertyDependencyTree>> returnedSymbolsByVars = new();

            info.Getter.DescendantNodes(node => WalkDownNodes(node, _dependencies, new())).Walk();

            bool WalkDownNodes(SyntaxNode node, PropertyDependencyTree currentNestedScope, (SyntaxNode, HashSet<PropertyDependencyTree>)? variableCollector = null)
            {
                switch (node)
                {
                    case AssignmentExpressionSyntax
                    {
                        Left: DeclarationExpressionSyntax {
                            Designation: ParenthesizedVariableDesignationSyntax {
                                Variables: { Count: > 0 and int count } vars } },
                        Right: TupleExpressionSyntax { Arguments: { } args } tupleRight
                    } declarator
                    :
                        for (int i = 0; i < count; i++)
                            RegisterVariable(currentNestedScope, vars[i], args[i]);
                        return false;
                    case VariableDesignationSyntax { Parent: RecursivePatternSyntax or DeclarationPatternSyntax } vd
                    :
                        returnedSymbolsByVars[vd] = new() { currentNestedScope };
                        break;
                    case VariableDeclaratorSyntax declarator
                    :
                        if (declarator.Initializer is { Value: { } value })
                            RegisterVariable(currentNestedScope, declarator, value);
                        return false;
                    case IsPatternExpressionSyntax isPattern
                    :
                        //currentScope.vars.Add(declarator.Initializer, new());
                        return ReadChainedMembers(isPattern.Expression, out var lastNode)
                            && WalkDownNodes(isPattern.Pattern, lastNode, variableCollector) && false;
                    case SubpatternSyntax subpattern
                    :
                        //assigningScopes.Push(subpattern.Pattern);
                        return ReadChainedMembers(subpattern.ExpressionColon?.Expression ?? subpattern.NameColon?.Name!, out var lastNode1)
                            && WalkDownNodes(subpattern.Pattern, lastNode1, variableCollector) && false;
                    case InterpolatedStringExpressionSyntax { Contents: { Count: > 0 } contents }
                    :
                        foreach (var item in contents)
                        {
                            if(item is InterpolationSyntax { Expression:{ } expr })
                                expr.DescendantNodes(n => WalkDownNodes(n, currentNestedScope, variableCollector)).Walk();
                        }
                        return false;
                    case InvocationExpressionSyntax { ArgumentList.Arguments: { } args, Expression: { } expr }
                    :
                        switch (expr)
                        {
                            case MemberAccessExpressionSyntax { Expression: { } subExpre }:
                                subExpre.DescendantNodes(n => WalkDownNodes(n, currentNestedScope, variableCollector)).Walk(); 
                                break;
                            case ConditionalAccessExpressionSyntax { Expression: { } subExpre }:
                                subExpre.DescendantNodes(n => WalkDownNodes(n, currentNestedScope, variableCollector)).Walk();
                                break;
                            default:
                                expr.DescendantNodes(n => WalkDownNodes(n, currentNestedScope, variableCollector)).Walk();
                                break;
                        }

                        foreach (var item in args)
                            item.Expression.DescendantNodes(n => WalkDownNodes(n, currentNestedScope, variableCollector)).Walk();
                        return false;
                    case MemberAccessExpressionSyntax or ConditionalAccessExpressionSyntax
                    :
                        return ReadChainedMembers(node, out var lastNode2) && false;
                    case IdentifierNameSyntax { IsVar: false } id
                    :
                        if (IsParentInvocation(node, info.Getter) || !IsNotifiableContainingType(_model, id, out var prop, out var type))
                            return true;

                        var newNode = currentNestedScope.AddDependencies(true, propName, prop);

                        if (variableCollector is ({ } n, { } ch))
                            ch.Add(newNode);

                        return false;
                }

                return true;

                bool ReadChainedMembers(SyntaxNode node, out PropertyDependencyTree lastNode)
                {
                    int i = -1;
                    SyntaxNode? varRef = default;
                    IdentifierNameSyntax? lastId = default;
                    PropertyDependencyTree? _lastNode = lastNode = default!;
                    var exit = false;
                    int parentCount = 0;
                    List<PropertyDependencyTree> parents = new();

                    node.DescendantNodes(subNode =>
                    {
                        if (exit || subNode is not IdentifierNameSyntax id)
                            return true;

                        var symbolInfo = _model.GetSymbolInfo(id);

                        i++;

                        if (IsParentInvocation(id, info.Getter))
                        {
                            return !(exit = true);
                        }
                        else if (ReturnsNotifiableType(symbolInfo.Symbol, out var propType))
                        {
                            varRef = symbolInfo.Symbol!.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax();
                            return exit = varRef == null;
                        }
                        else if (IsNotifiable(symbolInfo, out var prop, out var type))
                        {
                            lastId = id;

                            if (varRef != null && returnedSymbolsByVars.TryGetValue(varRef, out var _parents))
                            {
                                foreach (var item in _parents)
                                {
                                    parents.Add(item.AddDependencies(true, propName, prop));
                                    parentCount++;
                                }
                            }
                            else if (parentCount > 0)
                            {
                                for (int i = 0; i < parentCount; i++)
                                {
                                    parents[i] = parents[i].AddDependencies(true, propName, prop);
                                }
                            }
                            else
                            {
                                _lastNode = (_lastNode ?? currentNestedScope).AddDependencies(true, propName, prop);
                            }
                            //For multiple output variable (patterns, ternary and switch expressions)
                            //if (currentAssigning != null && currentScope.vars.TryGetValue(currentAssigning, out var parent))
                            //    foreach (var item in parent)
                            //        _lastNode = new Node(prop, item);
                            //else

                            return true;
                        }

                        return exit = lastId?.Span.End < node.Span.End;

                    }).Walk();

                    if (_lastNode != null)
                    {
                        if (variableCollector is ({ } n, { } ch))
                            ch.Add(_lastNode);
                        lastNode = _lastNode;
                    }

                    return !exit;
                }

                void RegisterVariable(PropertyDependencyTree currentNestedScope, SyntaxNode declarator, SyntaxNode value)
                {
                    SyntaxNode decl = declarator;
                    HashSet<PropertyDependencyTree> returned = new();

                    value.DescendantNodes(n => WalkDownNodes(n, currentNestedScope, (decl, returned))).Walk();

                    if (returned.Count > 0)
                        returnedSymbolsByVars[decl] = returned;
                }
            }
        }

        if (info is { IsImplemented: false, Ignore: false })
        {
            if (!info.IsReadOnly || info.IsCommand)
                _fields.AddNested(typeName, (fieldName, propInfo.Type.IsNullable(), propInfo.Type.AllowsNull()));

            if (!info.IsCommand)
                _needsNotifyMethod = true;
        }

        return info;


    }
    static bool ReturnsNotifiableType(ISymbol? symbol, out INamedTypeSymbol type) =>
        (type = ((symbol as ILocalSymbol)?.Type as INamedTypeSymbol)!) != null && IsNotifiableType(type);


    static bool IsParentInvocation(SyntaxNode x, SyntaxNode accRoot)
    {
        var parent2 = x.Parent!;
        var isParentInvoke2 = false;

        while (parent2 != null && parent2 != accRoot && parent2 is not StatementSyntax && !(isParentInvoke2 = parent2 is InvocationExpressionSyntax { Expression.Span.End: { } end } && end == x.Span.End))
        {
            parent2 = parent2?.Parent;
        }

        return isParentInvoke2;
    }

    private static bool IsNotifiable(IPropertySymbol id)
    {
        return id.ContainingType.HasAttribute(NAMESPACE, ATTRIBUTE);
    }

    private static string GetFieldName(string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return string.Empty;
        }
        char[] buffer = ArrayPool<char>.Shared.Rent(input.Length + 1);
        try
        {
            ('_' + input).CopyTo(0, buffer, 0, input.Length + 1);
            buffer[1] = char.ToLower(buffer[1]);
            return new string(buffer, 0, input.Length + 1);
        }
        finally
        {
            ArrayPool<char>.Shared.Return(buffer);
        }
    }
    public override string ToString()
    {
        _builder.AppendFormat(@"//<auto generated>
#nullable enable

namespace {0};

public partial class {1} : global::SourceCrafter.Mvvm.ViewModelBase, {2} 
{{", _namespace.Replace("global::", ""), ClassName, _interfaceName);

        BuildFields();

        BuildProperties();

        if (_dependencies.Count > 0)
        {
            _builder.Append(@"

    protected void NotifyChange(global::System.ComponentModel.PropertyChangedEventArgs evtArgs) 
    {
        OnPropertyChanged(evtArgs);");

            BuildSwitch(_builder, _dependencies);

            _builder.Append(@"
    }");
        }

        _builder.Append(@"
}");
        return _builder.ToString();

    }

    private void BuildFields()
    {
        int initLen = _builder.Length;

        //Build fields syntax
        _fields.Aggregate(_builder, (_, kv) =>
        {
            var start = _builder.AppendFormat(@"

    private {0}", kv.Key);

            initLen = _builder.Length;

            kv.Value.Aggregate(start, (_, fieldInfo) =>
            {
                if (_builder.Length > initLen)
                    _builder.Append(",");

                _builder.AppendFormat(@"
        {0}", fieldInfo.name);

                if (fieldInfo.allowsNull)
                {
                    _builder.Append(" = default");

                    if (!fieldInfo.isNullable)
                        _builder.Append("!");
                }

                return _builder;
            });

            return start.Append(";");
        });
    }

    void BuildProperties()
    {
        //Build properies
        foreach (var prop in _properties)
        {
            _builder.Append(@"
    ");
            var hasDependencies = _dependencies.TryGetValue(prop.Symbol, out var deps) && deps.NotifyTo.Any();

            if (prop.IsCommand)
            {
                BuildCommandProperty(prop.Type, prop.Name, prop.BackingField, prop.CommandInfo);
            }
            else
            {
                _builder.AppendFormat(@"
    public {0} {1}", prop.Type, prop.Name);

                if (prop.IsReadOnly && prop.IsSingleStatementGetter)
                {
                    _builder.Append($" => {prop.Getter};");
                    continue;
                }

                _builder.Append(" {");

                if (!prop.IsWriteOnly)
                {
                    _builder.Append($@"{(prop.IsReadOnly || prop.Ignore ? " " : @"
        ")}get");

                    if (prop.UseBackingField || prop.IsSingleStatementGetter)
                        _builder.Append(" =>");

                    _builder.Append($" {(prop.UseBackingField ? prop.BackingField : prop.Getter)}".TrimEnd());

                    if (!prop.IsImplemented || prop.IsSingleStatementGetter)
                        _builder.Append(";");


                    if (prop.IsReadOnly)
                    {
                        if (!prop.IsSingleStatementGetter)
                            _builder.Append(" }");
                        continue;
                    }
                }

                if (!prop.IsReadOnly)
                {
                    _builder.Append(prop.IsWriteOnly || prop.Ignore ? " " : @"
        ");
                    _builder.Append("set");

                    if (prop.Ignore)
                    {
                        if (prop.IsSingleStatementSetter)
                            _builder.Append(" =>");

                        _builder.Append($" {prop.Setter}".TrimEnd());

                        if (prop.IsSingleStatementSetter || prop.Getter == null)
                            _builder.Append(";");

                        if (!prop.IsImplemented)
                        {
                            _builder.Append(" }");
                            continue;
                        }
                    }
                    else
                    {
                        _builder.AppendFormat(@" {{
            if(Equals(value, {0}))
                return;
            ", prop.UseBackingField ? prop.BackingField : prop.Name);

                        if (prop.UseBackingField && !prop.IsWriteOnly)
                            _builder.AppendFormat("{0} = value;", prop.BackingField);

                        if (!prop.IsSingleStatementSetter && prop.Setter is BlockSyntax bs)
                            bs.Statements
                                .Aggregate(_builder, (sb, st) => sb
                                    .Append($@"
            {st}"));
                        else if (prop.Setter != null)
                            _builder.Append(prop.Setter)
                                .Append(";");

                        _builder.AppendFormat(@"
            {0}(new(""{1}""));", hasDependencies ? "NotifyChange" : "OnPropertyChanged", prop.Name);

                        _builder.Append(@"
        }");
                    }

                }

                _builder.Append(@"
    }");

            }
        }
    }

    private void BuildCommandProperty(string typeName, string propName, string fieldName, CommandInfo info)
    {
        string
            baseMethodName = $"Execute{propName[0..^7]}",
            methodName = info.IsAsync ? $"{baseMethodName}Async" : baseMethodName,
            parameterSyntax = typeName.IndexOf("<", StringComparison.Ordinal) is { } a and > 0 &&
                              typeName[a..(typeName.LastIndexOf('>') + 1)] is ['<', .. { Length: > 0 } genericArgumentName, '>']
                              ? $"{genericArgumentName} parameter"
                              : "";

        _builder.AppendFormat(@"
    public {0} {1} => {2} ??= new {0}({3}", typeName, propName, fieldName, methodName);

        if (info.CheckExec)
            _builder.AppendFormat(", Can{0}", baseMethodName);

        if (info.IsAsync)
            _builder.AppendFormat(", {0}", info.AsyncOption);

        _builder.Append(@");

    ");

        if (info.CheckExec)
            _builder.AppendFormat(@"private partial bool Can{0}({1});

    ", baseMethodName, parameterSyntax);

        _builder
            .AppendFormat(@"{0} {1}({2});
    ", info.IsAsync ? "private partial global::System.Threading.Tasks.Task" : "partial void", methodName, parameterSyntax);

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
                code.AppendFormat(@"
{0}         ({1} as global::SourceCrafter.Mvvm.IObservable)?.Subscribe((s{2}, e{2}) => {{", indentStr, prop.Name, level);

                BuildSwitch(code, deps, indent + 3, $"e{level}", level + 1);

                code.AppendFormat(@"
{0}         }});", indentStr);
            }

            foreach (var item in deps.NotifyTo)
            {
                code.AppendFormat(@"
{0}        OnPropertyChanged(new(""{1}""));", indentStr, item);
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
            var isCommand = propSymbol.Name.EndsWith("Command");
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

        public bool UseBackingField => !Ignore && !IsImplemented && !IsReadOnly;

        public IPropertySymbol Symbol { get; internal set; }
        public bool IsSingleStatementGetter { get; private set; }
        public bool IsSingleStatementSetter { get; private set; }

        private static CSharpSyntaxNode? TryReduceMethod(IMethodSymbol? method) =>

            (method?.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax() as AccessorDeclarationSyntax) switch
            {
                { ExpressionBody.Expression: { } expr } => expr,

                { Body.Statements: [{ } retValue] } => (CSharpSyntaxNode)retValue.ChildNodes().First(),

                var ret => ret?.Body
            };

        private static void GetCommandOptions(ImmutableArray<AttributeData> attributeDatas, out bool checkExec, out string asyncOption)
        {
            checkExec = true;
            asyncOption = "global::CommunityToolkit.Mvvm.Input.AsyncRelayCommandOptions.None";

            if (attributeDatas.FirstOrDefault("CommandOptions".IsAttributeName)?.ApplicationSyntaxReference?.GetSyntax() is AttributeSyntax
                {
                    ArgumentList.Arguments: [{ Expression: LiteralExpressionSyntax { Token.Value: bool check } }, ..] args
                })
            {
                checkExec = check;
                if (args.Count > 1)
                {
                    asyncOption = args[1].DescendantNodesAndSelf().OfType<MemberAccessExpressionSyntax>().Join(" | ");
                }
            }
        }
    }

    internal record struct CommandInfo(bool IsAsync, bool CheckExec, string? AsyncOption);


    static bool IsAssignableFrom(ITypeSymbol type, ITypeSymbol _base) =>
            SymbolEqualityComparer.Default.Equals(type, _base) ||
                type.AllInterfaces.Any(t => IsAssignableFrom(t, _base)) ||
                type.BaseType != null && IsAssignableFrom(type.BaseType, _base);

    static bool IsNotifiable(SymbolInfo id, out IPropertySymbol prop, out INamedTypeSymbol type)
        =>
            ((prop, type) = id switch
            {
                { Symbol: IPropertySymbol { } symbol }
                    when IsNotifiableContainingType<IPropertySymbol>(symbol, out var _type) =>
                        (symbol, _type),

                { CandidateSymbols: { Length: > 0 } cands }
                    when TryTakeOutCandidate<IPropertySymbol>(cands, out var _prop, out var _type)
                        => (_prop, _type),

                _ => (default!, default!)

            }) is ({ }, { });

    static bool IsNotifiableContainingType(SemanticModel model, IdentifierNameSyntax id, out IPropertySymbol prop, out INamedTypeSymbol type)
        => IsNotifiable(model.GetSymbolInfo(id), out prop, out type);

    static bool IsNotifiableContainingType<T>(ISymbol symbol, out INamedTypeSymbol type) where T : ISymbol =>
        IsNotifiableType(type = symbol.ContainingType);

    static bool TryTakeOutCandidate<T>(ImmutableArray<ISymbol> cands, out T symbol, out INamedTypeSymbol type) where T : ISymbol
    {
        symbol = default!;
        type = default!;

        foreach (var item in cands)
        {
            if (item is T t && IsNotifiableContainingType<T>(t, out type))
            {
                symbol = t;
                return true;
            }
        }

        return false;
    }

    static bool IsNotifiableType(ITypeSymbol type) =>
        type.GetAttributes()
            .Any(attr =>
                true == attr.AttributeClass
                    ?.ToString()
                    ?.StartsWith(string.Format("{0}.{1}", NAMESPACE, ATTRIBUTE)));

    internal class PropertyDependencyTree : Dictionary<IPropertySymbol, PropertyDependencyTree>
    {
        static readonly IdentifierComparer<IPropertySymbol> comparer = new(SymbolEqualityComparer.Default.Equals);

        public PropertyDependencyTree(SemanticModel model) : base(comparer) => _model = model;


        internal ImmutableHashSet<string>.Builder NotifyTo = ImmutableHashSet.CreateBuilder<string>(StringComparer.InvariantCulture);
        private SemanticModel _model;

        public IPropertySymbol? Property { get; private set; }

        internal PropertyDependencyTree AddDependencies(bool returnDeep, string notifyTo, IPropertySymbol children)
        {
            if (!TryGetValue(children, out var childDependency))
            {
                childDependency = this[children] = new(_model) { Property = children };
            }

            childDependency.NotifyTo.Add(notifyTo);

            return returnDeep ? childDependency : this;
        }
        public override string ToString()
        {
            return (Property?.Name ?? "{root}") + GetIndentedString();
        }

        private string GetIndentedString(int level = 1)
        {
            return string.Join(",",
                this.Select(kv => {
                    string indent = new string(' ', level * 2);
                    return '\n' + indent + kv.Key.Name +
                        (kv.Value.Count > 0
                            ? ": [" + kv.Value.GetIndentedString(level + 1) + '\n' + indent + "]"
                            : "") + (kv.Value.NotifyTo.Count > 0 ? " => (" + kv.Value.NotifyTo.Join(", ") + ')' : "");
                }));
        }
    }

    internal class IdentifierComparer<T> : IEqualityComparer<T> where T : notnull
    {
        private static Func<T?, T?, bool> _comparer = null!;

        internal IdentifierComparer(Func<T?, T?, bool> comparer) => _comparer = comparer;
        public bool Equals(T? x, T? y) => _comparer(x, y);

        public int GetHashCode(T obj) => obj.GetHashCode();
    }
}
