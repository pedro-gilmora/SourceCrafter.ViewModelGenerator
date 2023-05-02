using System.Diagnostics;
using CommunityToolkit.Mvvm.Input;
using SourceCrafter.Mvvm.Attributes;
using Xunit;

namespace SourceCrafter.ViewModelGenerator.UnitTests
{
    using FluentAssertions;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    public static class Test
    {
        [Fact]
        public static void ShouldGenerateAppManagerCode()
        {
            var root = CSharpSyntaxTree.ParseText(@"using SourceCrafter.Mvvm.Attributes;
namespace Test {
    [ObservableModel]
    public interface IAppManager
    {
        User? User => Authentication.User;

        IAuthentication Authentication { get; set; }

        bool IsAuthenticated => Authentication.Token is { };
    }

    [ObservableModel]
    public interface IAuthentication
    {
        string? Email { get; set; }
        string? Password { get; set; }
        string? Type { get; set; }
        string? Token { get; set; }
        bool IsBusy { get; set; }
        bool CanLogin => !IsBusy && !string.IsNullOrEmpty(Email?.Trim()) && !string.IsNullOrEmpty(Password?.Trim());
        bool CanLogout => !IsBusy && !string.IsNullOrEmpty(Token?.Trim());

        [CommandOptions(false)]
        AsyncRelayCommand LoginCommand { get; }
        [CommandOptions(false)]
        AsyncRelayCommand LogoutCommand { get; }
        IAppUser? User { get; set; }

        Task EnsureAuthorization();
    }

    [ObservableModel, Map<User>]
    public interface IAppUser
    {
        string UserName { get; set; }
        [Map(nameof(User.Name))]
        string FirstName { get; set; }
        string LastName { get; set; }
        string Logo { get; set; }
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
                var script = new ViewModelSyntaxGenerator(sm.GetDeclaredSymbol(iface)!, sm).ToString();
                Trace.WriteLine(script); 
            }
        }
        [Fact]
        public static void ShouldGenerateUserCode()
        {
            var root = CSharpSyntaxTree.ParseText(@"using SourceCrafter.Mvvm.Attributes;
namespace Test {
    public enum Role
    {
        Admin,
        Moderator,
        Guest,
        User
    }

    [ObservableModel]
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

    [ObservableModel]
    public interface IAction
    {
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
