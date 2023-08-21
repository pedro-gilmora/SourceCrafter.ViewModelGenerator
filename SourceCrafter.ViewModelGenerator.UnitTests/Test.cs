using Xunit;
using System.Diagnostics;
using CommunityToolkit.Mvvm.Input;
using SourceCrafter.Mvvm.Attributes;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Reflection;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.Text;
using FluentAssertions.Equivalency;
using System.Data;
using System.Diagnostics.CodeAnalysis;
using Xunit.Sdk;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.VisualBasic;
using System.Linq;

namespace SourceCrafter.ViewModelGenerator.UnitTests
{

    public static class Test
    {
        //[Fact]
//        public static void TestRandomCode()
//        {
//            var code = @"using SourceCrafter.Mvvm.Attributes;
//namespace Test 
//{
//  [SourceCrafter.Mvvm.Attributes.ObservableModel]
//  public interface IAppManager
//  {
//      User? User => Authentication?.User;

//      IAuthentication Authentication { get; set; }

//      bool IsAuthenticated => Authentication is { Token.Length: > 0, Email: { Length: > 0 } };
      
//      int Age { get; set; }

//      bool IsUnder18
//      {
//        get 
//        {
//          var (auth, age) = (Authentication, Age);

//          if(auth != null)
//            auth.Email = """";

//          return age >= 18;
//        }
//        set
//        {
//          if(value && Age > 18)
//            Age = 17;
//        }
//      }

//    }

//  [SourceCrafter.Mvvm.Attributes.ObservableModel]
//  public interface IAuthentication
//  {
//    string? User { get; set; }
//    string? Email { get; set; }
//    string? Password { get; set; }
//    string? Token { get; set; }
//    bool IsBusy { get; set; }
//    bool CanLogin => !IsBusy && !string.IsNullOrEmpty(Email?.Trim()) && !string.IsNullOrEmpty(Password?.Trim());
//    bool CanLogout => !IsBusy && !string.IsNullOrEmpty(Token);
//  }
//}";

//            var textCode = CSharpSyntaxTree.ParseText(code);
//            var sourceText = textCode.GetText();
//            var root = textCode.GetRoot();
//            var mscorlib = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
//            var compilation = CSharpCompilation.Create("MyCompilation", new[] { root.SyntaxTree }, new[] { mscorlib });
//            var model = compilation.GetSemanticModel(root.SyntaxTree)!;

//            foreach (var cls in root.DescendantNodes().OfType<InterfaceDeclarationSyntax>())
//            {
//                PropertyDependencyTree rootNode = new(model);

//                cls.DescendantNodes(node =>
//                {
//                    if (node is not PropertyDeclarationSyntax
//                        {
//                            Identifier.ValueText: { } propName,
//                            ExpressionBody: var exprBody,
//                            AccessorList: var accList
//                        })
//                        return true;

//                    var accRoot = exprBody ??
//                        (accList?.
//                            DescendantNodes().
//                            OfType<AccessorDeclarationSyntax>().
//                            FirstOrDefault(e => e.IsKind(SyntaxKind.GetAccessorDeclaration)) is
//                        {
//                            Body: var body,
//                            ExpressionBody: var getExprBody
//                        } acc
//                                    ? getExprBody ?? (SyntaxNode)body!
//                                    : null);

//                    if (accRoot == null)
//                        return false;

//                    Dictionary<SyntaxNode, HashSet<PropertyDependencyTree>> returnedSymbolsByVars = new();

//                    accRoot.DescendantNodes(node => WalkDownNodes(node, rootNode, new())).Walk();

//                    bool WalkDownNodes(SyntaxNode node, PropertyDependencyTree currentNestedScope, (SyntaxNode, HashSet<PropertyDependencyTree>)? variableCollector = null)
//                    {
//                        switch (node)
//                        {
//                            case AssignmentExpressionSyntax
//                            {
//                                Left: DeclarationExpressionSyntax
//                                {
//                                    Designation: ParenthesizedVariableDesignationSyntax
//                                    {
//                                        Variables: { Count: > 0 and int count } vars
//                                    }
//                                },
//                                Right: TupleExpressionSyntax { Arguments: { } args } tupleRight
//                            } declarator
//                            :
//                                for (int i = 0; i < count; i++)
//                                    RegisterVariable(currentNestedScope, vars[i], args[i]);

//                                return false;
//                            case VariableDesignationSyntax { Parent: RecursivePatternSyntax or DeclarationPatternSyntax } vd
//                            :
//                                returnedSymbolsByVars[vd] = new() { currentNestedScope };
//                                break;
//                            case VariableDeclaratorSyntax declarator
//                            :
//                                if (declarator.Initializer is { Value: { } value })
//                                    RegisterVariable(currentNestedScope, declarator, value);

//                                return false;
//                            case IsPatternExpressionSyntax isPattern
//                            :
//                                //currentScope.vars.Add(declarator.Initializer, new());
//                                return ReadChainedMembers(isPattern.Expression, out var lastNode)
//                                    && WalkDownNodes(isPattern.Pattern, lastNode, variableCollector) && false;
//                            case SubpatternSyntax subpattern
//                            :
//                                //assigningScopes.Push(subpattern.Pattern);
//                                return ReadChainedMembers(subpattern.ExpressionColon?.Expression ?? subpattern.NameColon?.Name!, out var lastNode1)
//                                    && WalkDownNodes(subpattern.Pattern, lastNode1, variableCollector) && false;
//                            case MemberAccessExpressionSyntax or ConditionalAccessExpressionSyntax
//                            :
//                                return ReadChainedMembers(node, out var lastNode2) && false;
//                            case IdentifierNameSyntax { IsVar: false } id
//                            :
//                                if (IsParentInvocation(node, accRoot) || !IsNotifiableContainingType(model, id, out var prop, out var type))
//                                    return true;

//                                var newNode = currentNestedScope.AddDependencies(true, propName, prop);

//                                if (variableCollector is ({ } n, { } ch))
//                                    ch.Add(newNode);

//                                return false;
//                        }

//                        return true;

//                        bool ReadChainedMembers(SyntaxNode node, out PropertyDependencyTree lastNode)
//                        {
//                            int i = -1;
//                            SyntaxNode? varRef = default;
//                            IdentifierNameSyntax? lastId = default;
//                            PropertyDependencyTree? _lastNode = lastNode = default!;
//                            IPropertySymbol _lastProp = default!;
//                            var exit = false;
//                            int parentCount = 0;
//                            List<PropertyDependencyTree> parents = new();
//                            node.DescendantNodes(subNode =>
//                            {
//                                if (exit || subNode is not IdentifierNameSyntax id)
//                                    return true;

//                                var symbolInfo = model.GetSymbolInfo(id);
//                                //List<IdentifierNameSyntax>
//                                i++;

//                                if (IsParentInvocation(id, accRoot))
//                                {
//                                    return !(exit = true);
//                                }
//                                else if (ReturnsNotifiableType(symbolInfo.Symbol, out var propType))
//                                {
//                                    varRef = symbolInfo.Symbol!.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax();
//                                    return exit = varRef == null;
//                                }
//                                else if (IsNotifiable(symbolInfo, out var prop, out var type))
//                                {
//                                    lastId = id;

//                                    if (varRef != null && returnedSymbolsByVars.TryGetValue(varRef, out var _parents))
//                                    {
//                                        foreach (var item in _parents)
//                                        {
//                                            parents.Add(item.AddDependencies(true, propName, prop));
//                                            parentCount++;
//                                        }
//                                    }
//                                    else if (parentCount > 0)
//                                    {
//                                        for (int i = 0; i < parentCount; i++)
//                                        {
//                                            parents[i] = parents[i].AddDependencies(true, propName, prop);
//                                        }
//                                    }
//                                    else
//                                    {
//                                        _lastNode = (_lastNode ?? currentNestedScope).AddDependencies(true, propName, _lastProp = prop);
//                                    }
//                                    //For multiple output variable (patterns, ternary and switch expressions)
//                                    //if (currentAssigning != null && currentScope.vars.TryGetValue(currentAssigning, out var parent))
//                                    //    foreach (var item in parent)
//                                    //        _lastNode = new Node(prop, item);
//                                    //else

//                                    return true;
//                                }

//                                return exit = lastId?.Span.End < node.Span.End;

//                            }).Walk();

//                            if (_lastNode != null)
//                            {
//                                if (variableCollector is ({ } n, { } ch))
//                                    ch.Add(_lastNode);
//                                lastNode = _lastNode;
//                            }

//                            return !exit;
//                        }

//                        void RegisterVariable(PropertyDependencyTree currentNestedScope, SyntaxNode declarator, SyntaxNode value)
//                        {
//                            SyntaxNode decl = declarator;
//                            HashSet<PropertyDependencyTree> returned = new();

//                            value.DescendantNodes(n => WalkDownNodes(n, currentNestedScope, (decl, returned))).Walk();

//                            if (returned.Count > 0)
//                                returnedSymbolsByVars[decl] = returned;
//                        }
//                    }

//                    return false;

//                }).Walk();
//            }

//            bool isReturnTypeSymbol(SyntaxNode? node, INamedTypeSymbol varType, out IPropertySymbol parent)
//            {
//                var _parent = parent = null!;
//                if (node switch
//                {
//                    ConditionalAccessExpressionSyntax { WhenNotNull: { } whenNotNull } =>
//                        isReturnTypeSymbol(whenNotNull, varType, out _parent),

//                    ConditionalExpressionSyntax c =>
//                        isReturnTypeSymbol(c.WhenTrue, varType, out _parent) || isReturnTypeSymbol(c.WhenFalse, varType, out _parent),

//                    SwitchExpressionSyntax s => s.Arms.Any(sa =>
//                        isReturnTypeSymbol(sa.Expression, varType, out _parent)),

//                    MemberAccessExpressionSyntax { Name: { } name } =>
//                        model.GetSymbolInfo(name) is { Symbol: IPropertySymbol { } symbol }
//                            && IsAssignableFrom((_parent = symbol).Type, varType),

//                    IdentifierNameSyntax name =>
//                        model.GetSymbolInfo(name) is { Symbol: IPropertySymbol { ContainingType: { } type } }
//                        && SymbolEqualityComparer.Default.Equals(type, varType),

//                    _ => default,
//                })
//                {
//                    parent = _parent;
//                    return true;
//                }
//                return false;
//            }
//        }

//        static bool IsAssignableFrom(ITypeSymbol type, ITypeSymbol _base) =>
//            SymbolEqualityComparer.Default.Equals(type, _base) ||
//                type.AllInterfaces.Any(t => IsAssignableFrom(t, _base)) ||
//                type.BaseType != null && IsAssignableFrom(type.BaseType, _base);

//        static bool IsNotifiable(SymbolInfo id, out IPropertySymbol prop, out INamedTypeSymbol type)
//            =>
//                ((prop, type) = id switch
//                {
//                    { Symbol: IPropertySymbol { } symbol }
//                        when IsNotifiableContainingType<IPropertySymbol>(symbol, out var _type) =>
//                            (symbol, _type),

//                    { CandidateSymbols: { Length: > 0 } cands }
//                        when TryTakeOutCandidate<IPropertySymbol>(cands, out var _prop, out var _type)
//                            => (_prop, _type),

//                    _ => (default!, default!)

//                }) is ({ }, { });

//        static bool IsNotifiableContainingType(SemanticModel model, IdentifierNameSyntax id, out IPropertySymbol prop, out INamedTypeSymbol type)
//            => IsNotifiable(model.GetSymbolInfo(id), out prop, out type);

//        static bool IsNotifiableContainingType<T>(ISymbol symbol, out INamedTypeSymbol type) where T : ISymbol =>
//            IsNotifiableType(type = symbol.ContainingType);

//        static bool ReturnsNotifiableType(ISymbol? symbol, out INamedTypeSymbol type) =>
//            (type = ((symbol as ILocalSymbol)?.Type as INamedTypeSymbol)!) != null && IsNotifiableType(type);

//        static bool TryTakeOutCandidate<T>(ImmutableArray<ISymbol> cands, out T symbol, out INamedTypeSymbol type) where T : ISymbol
//        {
//            symbol = default!;
//            type = default!;

//            foreach (var item in cands)
//            {
//                if (item is T t && IsNotifiableContainingType<T>(t, out type))
//                {
//                    symbol = t;
//                    return true;
//                }
//            }

//            return false;
//        }

//        const string
//            NAMESPACE = "SourceCrafter.Mvvm.Attributes",
//            ATTRIBUTE = "ObservableModel";

//        static bool IsNotifiableType(ITypeSymbol type) =>
//            type.GetAttributes()
//                .Any(attr =>
//                    true == attr.AttributeClass
//                        ?.ToString()
//                        ?.StartsWith(string.Format("{0}.{1}", NAMESPACE, ATTRIBUTE)));

//        static bool IsParentInvocation(SyntaxNode x, SyntaxNode accRoot)
//        {
//            var parent2 = x.Parent!;
//            var isParentInvoke2 = false;

//            while (parent2 != null && parent2 != accRoot && parent2 is not StatementSyntax && !(isParentInvoke2 = parent2 is InvocationExpressionSyntax { Expression.Span.End: { } end } && end == x.Span.End))
//            {
//                parent2 = parent2?.Parent;
//            }

//            return isParentInvoke2;
//        }

//        internal class PropertyDependencyTree : Dictionary<IPropertySymbol, PropertyDependencyTree>
//        {
//            static readonly IdentifierComparer<IPropertySymbol> comparer = new(SymbolEqualityComparer.Default.Equals);

//            public PropertyDependencyTree(SemanticModel model) : base(comparer) => _model = model;


//            internal ImmutableHashSet<string>.Builder NotifyTo = ImmutableHashSet.CreateBuilder<string>(StringComparer.InvariantCulture);
//            private SemanticModel _model;

//            public IPropertySymbol? Property { get; private set; }

//            internal PropertyDependencyTree AddDependencies(bool returnDeep, string notifyTo, IPropertySymbol children)
//            {
//                if (!TryGetValue(children, out var childDependency))
//                {
//                    childDependency = this[children] = new(_model) { Property = children };
//                }

//                childDependency.NotifyTo.Add(notifyTo);

//                return returnDeep ? childDependency : this;
//            }
//            public override string ToString()
//            {
//                return (Property?.Name ?? "{root}") + GetIndentedString();
//            }

//            private string GetIndentedString(int level = 1)
//            {
//                return string.Join(",", 
//                    this.Select(kv => {
//                        string indent = new string(' ', level * 2);
//                        return '\n' + indent + kv.Key.Name + 
//                            (kv.Value.Count > 0
//                                ? ": [" + kv.Value.GetIndentedString(level + 1) + '\n' + indent +"]"
//                                : "") + (kv.Value.NotifyTo.Count > 0 ? " => (" + kv.Value.NotifyTo.Join(", ") + ')' : "");
//                    }));
//            }
//        }

//        internal class IdentifierComparer<T> : IEqualityComparer<T> where T : notnull
//        {
//            private static Func<T?, T?, bool> _comparer = null!;

//            internal IdentifierComparer(Func<T?, T?, bool> comparer) => _comparer = comparer;
//            public bool Equals(T? x, T? y) => _comparer(x, y);

//            public int GetHashCode([DisallowNull] T obj) => obj.GetHashCode();
//        }
        
        [Fact]
        public static void ShouldGenerateAppManagerCode()
        {
            var root = CSharpSyntaxTree.ParseText(@"using SourceCrafter.Mvvm.Attributes;
    namespace Test 
    {
    [SourceCrafter.Mvvm.Attributes.ObservableModel]
    public interface IAppManager
    {
        User? User => Authentication?.User;

        IAuthentication Authentication { get; set; }

        bool IsAuthenticated => Authentication is { Token.Length: > 0, Email: { Length: > 0 } };

        int Age { get; set; }

        bool IsUnder18
        {
            get {
                var res = Age >= 18;
                var auth = Authentication;

                if(res)
                    auth.Email = """";

                return res;
            }
            set
            {
                if(value && Age > 18)
                    Age = 17;
            }
        }

    }

    [SourceCrafter.Mvvm.Attributes.ObservableModel]
    public interface IAuthentication
    {
        string? Email { get; set; }
        string? Password { get; set; }
        string? Type { get; set; }
        string? Token { get; set; }
        bool IsBusy { get; set; }
        bool CanLogin => !IsBusy && !string.IsNullOrEmpty(Email?.Trim()) && !string.IsNullOrEmpty(Password?.Trim());
        bool CanLogout => !IsBusy && !string.IsNullOrEmpty(Token);

        [CommandOptions(false)]
        AsyncRelayCommand LoginCommand { get; }
        [CommandOptions(false)]
        AsyncRelayCommand LogoutCommand { get; }
        IAppUser? User { get; set; }

        Task EnsureAuthorization();
    }

    [SourceCrafter.Mvvm.Attributes.ObservableModel, SourceCrafter.Mvvm.Attributes.Map<User>]
    public interface IAppUser
    {
        string UserName { get; set; }
        string FirstName { get; set; }
        string LastName { get; set; }
        string Logo { get; set; }
    }
    }").GetRoot();
            var e = CSharpCompilation.Create("Test",
                new[] { root.SyntaxTree },
                new[] {
                    MetadataReference.CreateFromFile(typeof(AsyncRelayCommandOptions).Assembly.Location),
                    MetadataReference.CreateFromFile(typeof(ReactiveAttribute).Assembly.Location),
                    //MetadataReference.CreateFromFile(typeof(CommandOptionsAttribute).Assembly.Location)
                });

            var sm = e.GetSemanticModel(root.SyntaxTree);

            //for (int i = 0; i < 3; i++)
            //{
            foreach (var iFace in root.DescendantNodes().OfType<InterfaceDeclarationSyntax>())
            {
                //var sw = Stopwatch.StartNew();
                var code = new ViewModelSyntaxGenerator(sm.GetDeclaredSymbol(iFace)!, sm).ToString();
                //Trace.WriteLine(sw.ElapsedMilliseconds);
                Trace.WriteLine(code);
            }
            //}
        }

        [Fact]
        public static void ShouldGenerateUserCode()
        {
            var root = CSharpSyntaxTree.ParseText(@"using SourceCrafter.Mvvm.Attributes;
    namespace Test 
    {
    public enum Role
    {
        Admin,
        Moderator,
        Guest,
        User
    }

    [SourceCrafter.Mvvm.Attributes.Reactive]
    public interface IUser
    {
        string ActionName => $""Running action: {Action.Name}"";
        IAction Action { get; set; }
        string FirstName { get; set; }
        string? LastName { get; set; }
        string Name => $""{FirstName} {LastName}"".Trim();
        bool Is18 { get => Age == 18; set => Age = value ? 18 : Age; }
        int Age { get; set; }
        bool CanDrink { get; set; }
        bool IsUnder18
        {
            get => Age >= 18;
            set
            {
                Age = value ? 18 : 17;
                if (IsUnder18) CanDrink = true;
                else CanDrink = false;
            }
        }
        RelayCommand<Role> SaveCommand { get; }
    }

    [SourceCrafter.Mvvm.Attributes.Reactive]
    public interface IAction
    {
        string Name { get; set; }
        bool ClearBrowserData { get; } 
    }
}").GetRoot();
            var e = CSharpCompilation.Create("Test",
                new[] { root.SyntaxTree },
                new[] {
                    MetadataReference.CreateFromFile(typeof(AsyncRelayCommandOptions).Assembly.Location),
                    //MetadataReference.CreateFromFile(typeof(CommandOptionsAttribute).Assembly.Location)
                });

            var sm = e.GetSemanticModel(root.SyntaxTree);

            foreach (var iface in root.DescendantNodes().OfType<InterfaceDeclarationSyntax>())
            {
                var script = new ViewModelSyntaxGenerator(sm.GetDeclaredSymbol(iface)!, sm).ToString();
                Trace.WriteLine(script);
            }
        }

        [Fact]
        public static void ShouldGenerateUser()
        {
            //User r = new() { };
            //int i = 0;
            //r.PropertyChanged += (o, e) =>
            //{
            //    switch (i++)
            //    {
            //        case 0:
            //            e.PropertyName.Should().Be(nameof(IUser.FirstName));
            //            break;
            //        case 1:
            //            e.PropertyName.Should().Be(nameof(IUser.Name));
            //            break;
            //        case 2:
            //            e.PropertyName.Should().Be(nameof(IUser.LastName));
            //            break;
            //        case 3:
            //            e.PropertyName.Should().Be(nameof(IUser.Name));
            //            break;
            //        case 4:
            //            e.PropertyName.Should().Be(nameof(IUser.Age));
            //            break;
            //        case 5:
            //            e.PropertyName.Should().Be(nameof(IUser.Is18));
            //            r.Is18.Should().BeFalse();
            //            break;
            //        case 6:
            //            e.PropertyName.Should().Be(nameof(IUser.IsUnder18));
            //            r.IsUnder18.Should().BeTrue();
            //            break;
            //        case 7:
            //            e.PropertyName.Should().Be(nameof(IUser.Age));
            //            break;
            //        case 8:
            //            e.PropertyName.Should().Be(nameof(IUser.Is18));
            //            r.Is18.Should().BeTrue();
            //            break;
            //        case 9:
            //            e.PropertyName.Should().Be(nameof(IUser.IsUnder18));
            //            r.IsUnder18.Should().BeFalse();
            //            break;
            //    }
            //};
            //r.FirstName = "Pedro";
            //r.LastName = "Gil";
            //r.Age = 17;
            //r.Age = 18;
            //r.Name.Should().Be("Pedro Gil");
        }
    }
}