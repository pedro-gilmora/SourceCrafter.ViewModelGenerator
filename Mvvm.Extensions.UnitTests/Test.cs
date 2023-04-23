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
    //[ObservableModel]
    //public interface IUser
    //{
    //    string Name { get; set; }
    //    int Age { get; set; }
	   // int NameLength => Name.Length;
    //    IAddress Address { get; }
    //    string State { set => Address.State = value; }
    //    bool IsAdult => this is { Age: >= 18 };
    //    bool IsJohnDoe => this is { Name: ""John Doe"", Age: >= 18 };
    //    bool LivesInNY => this is { Address: { State: { Length: 2 } } };
    //    bool IsJohnDoeInNY => this is { Name: ""John Doe"", Age: >= 18, Address.State: ""NY"" } };
    //}
    //[ObservableModel]
    //public interface IAddress
    //{
    //    string Street { get; }
    //    string City { get; }
    //    string State { get; }
    //}

    //[ObservableModel]
    //public interface IAuthentication
    //{
	   // bool CanLogin => this is {IsBusy: false, Email.Length: > 0, Password.Length: > 0};
	   // string? Email { get; set; }
	   // string? Password { get; set; }
	   // string? Token { get; set; }
	   // bool IsBusy { get; set; }
    //}

    [ObservableModel]
    public interface IUser
    {
        string ActionName => $""Action is called: {Action.Name}"";
        IAction Action { get; set; }
        string? LastName { get; set; }
        string FirstName { get; set; }
        string Name => $""{FirstName} {LastName}"".Trim();
        bool Is18 { get { return Age == 18; } set => Age = value ? 18 : Age; }
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
            
            foreach (var iface in root.DescendantNodes().OfType<InterfaceDeclarationSyntax>())
            {                
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
