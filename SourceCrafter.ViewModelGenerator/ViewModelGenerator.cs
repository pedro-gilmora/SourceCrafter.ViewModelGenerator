#nullable enable
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace SourceCrafter;

[Generator]
public class ViewModelGenerator : IIncrementalGenerator
{
    private static readonly object Lock = new();

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        lock (Lock)
        {
            context.RegisterSourceOutput(
                context.SyntaxProvider.ForAttributeWithMetadataName(
                    $"{ViewModelSyntaxGenerator.NAMESPACE}.{ViewModelSyntaxGenerator.ATTRIBUTE}",
                    static (n, _) => n is InterfaceDeclarationSyntax,
                    static (ctx, _) => (Interface: (ITypeSymbol)ctx.TargetSymbol, Model: ctx.SemanticModel)
                ),
                static (sourceProducer, interfaceToGenerate) =>
                {
                    var result = new ViewModelSyntaxGenerator(interfaceToGenerate.Interface, interfaceToGenerate.Model);
                    sourceProducer.AddSource(result.FileName, result.ToString());
                }
            );
        }
    }
}