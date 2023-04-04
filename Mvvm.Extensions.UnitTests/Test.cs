using System.Diagnostics;
using CommunityToolkit.Mvvm.Input;
using Mvvm.Extensions.Generator;
using Mvvm.Extensions.Generator.Attributes;
using Xunit;

namespace Mvvm.Extensions.UnitTests
{
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    public static class Test
    {
        [Fact]
        public static void ShouldGenerateCode()
        {
            var root = CSharpSyntaxTree.ParseText(@"using Mvvm.Extensions.Generator.Attributes;
namespace Test {

    [ObservableModel]
    public interface IAppManager
    {
        IUser? User { get; set; }

        IAuthentication Authentication { get; set; }

        bool IsAuthenticated => Authentication.Token is { };
    }

    [ObservableModel]
    public interface IAuthentication
    {
        string? Email { get; set; }
        string? Password { get; set; }
        string? Token { get; set; }
        bool IsBusy { get; set; }
        bool CanLogin => !IsBusy && !string.IsNullOrEmpty(Email?.Trim()) && !string.IsNullOrEmpty(Password?.Trim());

        [CommandOptions(false)]
        AsyncRelayCommand LoginCommand { get; }
        [CommandOptions(false)]
        AsyncRelayCommand LogoutCommand { get; }
    }

    [ObservableModel]
    public interface IUser
    {
        string ActionName => $""Action is called: {Action.Name}"";
        IAction Action { get; set; }
        string FirstName { get; set; }
        [Ignore]
        string? LastName { get; set; }
        string Name => $""{FirstName} {LastName}"".Trim();
        string AgeStatus => $""You're {(Is18 ? """" : ""not "")}old enough"";
        int Age { get;set }
        bool Is18 { 
            get => Age == 18; 
            set { 
                if(value)
                    Age = 18;
            }
        }
        AsyncRelayCommand<string?> AddManagerCommand { get; }
    }

    [ObservableModel]
    public interface IAction {
        string Name { get; set; }
    }
}").GetRoot();
            var e = CSharpCompilation.Create("Test", 
                new[] { root.SyntaxTree }, 
                new[] {
                    MetadataReference.CreateFromFile(typeof(AsyncRelayCommandOptions).Assembly.Location),
                    MetadataReference.CreateFromFile(typeof(CommandOptionsAttribute).Assembly.Location)
                });
            var sm = e.GetSemanticModel(root.SyntaxTree);
            foreach (var iface in root.DescendantNodes().OfType<InterfaceDeclarationSyntax>()){
                var script = new ViewModelGeneratorSyntax(sm.GetDeclaredSymbol(iface)!, sm).ToString();
                Trace.WriteLine(script); 
            }
        }

        [Fact]
        public static void ShouldGenerateUser()
        {
            User r = new() { };
            //int i = 0;
            //r.PropertyChanged += async (o, e) =>
            //{
            //    await Task.Delay(500);
            //    switch (i++)
            //    {
            //        case 0:
            //            e.PropertyName.Should().Be(nameof(IUser.FirstName));
            //            break;
            //        case 1:
            //            e.PropertyName.Should().Be(nameof(IUser.Name));
            //            break;
            //    }
            //};
            //r.FirstName = "Pedro";
            //r.LastName = "Gil";
            //r.Name.Should().Be("Pedro Gil");
        }
    }
}
