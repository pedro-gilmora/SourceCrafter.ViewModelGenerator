using CommunityToolkit.Mvvm.Input;
using FluentAssertions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Mvvm.Extensions.Generator;
using Mvvm.Extensions.Generator.Attributes;
using Xunit;

namespace Mvvm.Extensions.UnitTests
{
    public static class Test
    {
        [Fact]
        public static void ShouldGenerateCode()
        {
            var root = CSharpSyntaxTree.ParseText(@"public interface IUser
{
    string FirstName { get; set; }
    [Ignore]
    string? LastName { get; set; }
    string Name => $""{FirstName} {LastName}"".Trim();
    int Age { get;set }
    bool CanDrink { get;set }
    bool Is18 { get => Age == 18; set => Age = value ? 18 : Age; }
    bool IsUnder18 { 
        get => Age >= 18; 
        set { 
            Age = value ? 18 : 17; 
            if(IsUnder18) 
                CanDrink = true; 
            else 
                CanDrink = false; 
        } 
    }
    AsyncRelayCommand<string?> AddParentCommand { get; }
}").GetRoot();
            var e = CSharpCompilation.Create("Test", 
                new[] { root.SyntaxTree }, 
                new[] {
                    MetadataReference.CreateFromFile(typeof(AsyncRelayCommandOptions).Assembly.Location),
                    MetadataReference.CreateFromFile(typeof(CommandOptionsAttribute).Assembly.Location)
                });
            var sm = e.GetSemanticModel(root.SyntaxTree);
            ITypeSymbol iface = sm.GetDeclaredSymbol(root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().First())!;
            var (code, script) = ViewModelGenerator.CreateRelatedTypeFiles(iface, sm);
        }

        [Fact]
        public static void ShouldGenerateUser()
        {
            User r = new() { };
            int i = 0;
            r.PropertyChanged += (o, e) =>
            {
                switch (i++)
                {
                    case 0:
                        e.PropertyName.Should().Be(nameof(IUser.FirstName));
                        break;
                    case 1:
                        e.PropertyName.Should().Be(nameof(IUser.Name));
                        break;
                }
            };
            r.FirstName = "Pedro";
            r.LastName = "Gil";
            r.Name.Should().Be("Pedro Gil");
        }
    }
}
