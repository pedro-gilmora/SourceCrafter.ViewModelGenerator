using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Runtime.CompilerServices;
using System.Buffers;
using System.ComponentModel;
using SourceCrafter.Mvvm;

[assembly: InternalsVisibleTo("SourceCrafter.ViewModelGenerator.UnitTests")]

namespace SourceCrafter.Mvvm;

internal sealed partial class ViewModelSyntaxGenerator
{
    readonly PropertyDependencyTree _dependencies;

    readonly string _namespace;
    private readonly string _fullName;
    private Action? _buildProperties;

    public readonly string _className;

    public readonly string FileName;
    private readonly SemanticModel _model;
    private readonly StringBuilder _builder;

    //private static readonly Func<ISymbol?, ISymbol?, bool> AreSymbolsEquals = SymbolEqualityComparer.Default.Equals;
    internal const string NAMESPACE = "SourceCrafter.Mvvm.Attributes";
    internal const string ATTRIBUTE = "Reactive";

    public ViewModelSyntaxGenerator(ITypeSymbol abstractClass, SemanticModel model)
    {
        _model = model;
        _namespace = abstractClass.ContainingNamespace.ToGlobalNamespace().Replace("global::", "");
        _fullName = abstractClass.ToGlobalNamespace();
        _dependencies = new(model);

        _className = abstractClass.ToNameOnly();

        GetAllMembers(abstractClass);

        FileName = _fullName.Replace("global::", "") + ".g";

        _builder = new StringBuilder();
    }

    private void GetAllMembers(ITypeSymbol interfaceSymbol)
    {
        foreach (var item in interfaceSymbol.GetMembers())
            if (item is IPropertySymbol prop)
                CollectPropertyInfo(prop);
    }

    private void CreateField(int initLen, ITypeSymbol type, string fieldName)
    {
        bool isNullable = type.IsNullable(),
            allowsNull = type.AllowsNull();

        if (_builder.Length > initLen)
            _builder.Append(",");

        _builder.AppendFormat(@"
        {0}", fieldName);

        if (allowsNull)
        {
            _builder.Append(" = default");

            if (!isNullable)
                _builder.Append("!");
        }
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

    //private static bool IsNotifiable(IPropertySymbol id)
    //{
    //    return id.ContainingType.HasAttribute(NAMESPACE, ATTRIBUTE);
    //}

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

namespace {0}.Implementation;

public partial class {1} : {2} 
{{", _namespace, _className, _fullName);

        _buildProperties?.Invoke();

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

    private void BuildProperty(PropertySyntaxInfo prop)
    {
        _builder.Append(@"
    "); ;

        _builder.AppendFormat(@"
    public new {0} {1}", prop.Type, prop.Name);

        if (prop.IsReadOnly && prop.IsSingleStatementGetter)
        {
            _builder.Append($" => {prop.Getter};");
            return;
        }

        _builder.Append(" {");

        if (!prop.IsWriteOnly)
        {
            _builder.Append($@"{(prop.IsReadOnly || prop.Ignore ? " " : @"
        ")}get");

            if (prop.UseBackingField || prop.IsSingleStatementGetter)
                _builder.Append(" =>");

            _builder.Append($" base.{prop.Name}".TrimEnd());

            if (!prop.IsImplemented || prop.IsSingleStatementGetter)
                _builder.Append(";");


            if (prop.IsReadOnly)
            {
                if (!prop.IsSingleStatementGetter)
                    _builder.Append(" }");
                return;
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
                    return;
                }
            }
            else
            {
                string backingValue = "base." + prop.Name;

                _builder.AppendFormat(@" {{
            if(Equals(value, {0}))
                return;
            ", backingValue);

                if (!prop.IsWriteOnly)
                    _builder.AppendFormat("{0} = value;", backingValue);

                _builder.AppendFormat(@"
            {0}(new(""{1}""));", hasDependencies(prop) ? "NotifyChange" : "OnPropertyChanged", prop.Name);

                _builder.Append(@"
        }");
            }

        }

        _builder.Append(@"
    }");

        bool hasDependencies(PropertySyntaxInfo prop)
        {
            return _dependencies.TryGetValue(prop.Symbol, out var deps) && deps.NotifyTo.Count > 0;
        }
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

    internal readonly record struct PropertySyntaxInfo(string Type, string Name, string BackingField, bool IsImplemented, CSharpSyntaxNode? Getter, CSharpSyntaxNode? Setter, bool IsReadOnly, bool IsWriteOnly, bool Ignore)
    {
        internal static PropertySyntaxInfo Create(IPropertySymbol propSymbol, string typeName, string propName, string fieldName, bool isAutoProperty)
        {
            var (isImplemented, isReadOnly, isWriteOnly, getter, setter, ignore) = propSymbol switch
            {
                { IsIndexer: false, DeclaringSyntaxReferences: [.., { } propSyntaxRef] }
                    when propSyntaxRef.GetSyntax() is PropertyDeclarationSyntax { ExpressionBody.Expression: { } body } =>
                        (true, true, false, body, null, propSymbol.ContainsAttribute("Ignore")),

                { IsIndexer: false, GetMethod: var getMethod, SetMethod: var setMethod, IsReadOnly: var isRo, IsWriteOnly: var isWo } =>
                    (!isAutoProperty,
                     isRo,
                     isWo,
                     TryReduceMethod(getMethod),
                     TryReduceMethod(setMethod),
                     propSymbol.ContainsAttribute("Ignore")),

                _ => default
            };

            return new(typeName, propName, fieldName, isImplemented, getter, setter, isReadOnly, isWriteOnly, ignore)
            {
                Symbol = propSymbol,
                UseBackingField = !(ignore || isImplemented || isReadOnly),
                IsReactive = !(ignore || isReadOnly || propSymbol.IsVirtual && isWriteOnly),
                IsSingleStatementGetter = getter is { } and not BlockSyntax,
                IsSingleStatementSetter = setter is { } and not BlockSyntax
            };
        }

        internal readonly bool UseBackingField { get; init; }

        internal readonly IPropertySymbol Symbol { get; init; }
        internal readonly bool IsSingleStatementGetter { get; init; }
        internal readonly bool IsSingleStatementSetter { get; init; }
        internal readonly bool IsReactive { get; init; }

        private static CSharpSyntaxNode? TryReduceMethod(IMethodSymbol? method) =>

            (method?.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax() as AccessorDeclarationSyntax) switch
            {
                { ExpressionBody.Expression: { } expr } => expr,

                { Body.Statements: [{ } retValue] } => (CSharpSyntaxNode)retValue.ChildNodes().First(),

                var ret => ret?.Body
            };
    }

    static bool IsAssignableFrom(ITypeSymbol type, ITypeSymbol _base) =>
            SymbolEqualityComparer.Default.Equals(type, _base) ||
                type.AllInterfaces.Any(t => IsAssignableFrom(t, _base)) ||
                type.BaseType != null && IsAssignableFrom(type.BaseType, _base);

    static bool IsNotifiable(SymbolInfo id, out ISymbol prop, out INamedTypeSymbol type)
    {
        switch (id) 
        {
            case { Symbol: IPropertySymbol { } symbol } when IsNotifiableContainingType<IPropertySymbol>(symbol, out var _type) :
                (prop, type) = (symbol, _type);
                 return true;
            case { Symbol: IFieldSymbol { } symbol2 } when IsNotifiableContainingType<IFieldSymbol>(symbol2, out var _type) :
                (prop, type) = (symbol2, _type);
                return true;
            case { CandidateSymbols: { Length: > 0 } cands } when TryTakeOutCandidate<IPropertySymbol>(cands, out var _prop, out var _type) :
                (prop, type) = (_prop, _type);
                return true;
            default: 
                (prop, type) = (default!, default!);
                return false;
        };
    }

    static bool IsNotifiableContainingType(SemanticModel model, IdentifierNameSyntax id, out ISymbol prop, out INamedTypeSymbol type)
        => IsNotifiable(model.GetSymbolInfo(id), out prop, out type);

    static bool IsNotifiableContainingType<T>(ISymbol symbol, out INamedTypeSymbol type) where T : ISymbol 
        => IsNotifiableType(type = symbol.ContainingType);

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

    internal class PropertyDependencyTree(SemanticModel model) : Dictionary<ISymbol, PropertyDependencyTree>(comparer)
    {
        static readonly IdentifierComparer<ISymbol> comparer = new(SymbolEqualityComparer.Default.Equals);
        internal ImmutableHashSet<string>.Builder NotifyTo = ImmutableHashSet.CreateBuilder<string>(StringComparer.InvariantCulture);

        public ISymbol? Member { get; private set; }

        internal PropertyDependencyTree AddDependencies(bool returnDeep, string notifyTo, ISymbol children)
        {
            if (!TryGetValue(children, out var childDependency))
            {
                childDependency = this[children] = new(model) { Member = children };
            }

            childDependency.NotifyTo.Add(notifyTo);

            return returnDeep ? childDependency : this;
        }
        public override string ToString()
        {
            return (Member?.Name ?? "{root}") + GetIndentedString();
        }

        private string GetIndentedString(int level = 1)
        {
            return string.Join(",",
                this.Select(kv =>
                {
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

    private class FieldAggregatorComparer : IEqualityComparer<TypeFields>
    {
        public bool Equals(TypeFields x, TypeFields y)
        {
            if (x.TypeName.Equals(y.TypeName))
            {
                x.Builders += y.Builders;
                return true;
            }
            return false;
        }

        public int GetHashCode(TypeFields obj) => obj.TypeName.GetHashCode();
    }
}