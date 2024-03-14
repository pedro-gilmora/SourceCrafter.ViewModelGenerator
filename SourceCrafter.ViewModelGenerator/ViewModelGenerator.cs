#nullable enable
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SourceCrafter.Mvvm;

using System.Diagnostics;
using System.Linq;

namespace SourceCrafter;

[Generator]
internal class ViewModelGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        context.RegisterSourceOutput(
           context.SyntaxProvider.ForAttributeWithMetadataName(
                $"{ViewModelSyntaxGenerator.NAMESPACE}.{ViewModelSyntaxGenerator.ATTRIBUTE}Attribute",
                static (n, _) => 
                    n is ClassDeclarationSyntax { Modifiers: { } list } 
                    && list.Any(t => t.IsKind(Microsoft.CodeAnalysis.CSharp.SyntaxKind.AbstractKeyword)),
                static (ctx, _) => ((ITypeSymbol)ctx.TargetSymbol, Model: ctx.SemanticModel)
            ).Collect(),
            static (sourceProducer, interfacesToGenerate) =>
            {
//#if DEBUG
//                Debugger.Launch();
//#endif
                foreach (var (_class, model) in interfacesToGenerate)
                {
                    try
                    {
                        var result = new ViewModelSyntaxGenerator(_class, model);
                        sourceProducer.AddSource(result.FileName, result.ToString());
                    }
                    catch (System.Exception e)
                    {
                        sourceProducer.AddSource(_class.Name + ".error.cs", "/*" + e.ToString() + "*/");
                    }
                }
            }
        );
    }
}