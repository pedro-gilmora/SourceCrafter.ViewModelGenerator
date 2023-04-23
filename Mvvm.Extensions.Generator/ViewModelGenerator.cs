#nullable enable
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections;
using System.Linq;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using System.Collections.Immutable;
using System.Text;

namespace Mvvm.Extensions.Generator;

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
                    $"{ViewModelGeneratorSyntax.NAMESPACE}.{ViewModelGeneratorSyntax.ATTRIBUTE}",
                    static (n, _) => n is InterfaceDeclarationSyntax,
                    static (ctx, _) => (Interface: (ITypeSymbol)ctx.TargetSymbol, Model: ctx.SemanticModel)
                ),
                static (sourceProducer, interfaceToGenerate) =>
                {
                    var result = new ViewModelGeneratorSyntax(interfaceToGenerate.Interface, interfaceToGenerate.Model);
                    sourceProducer.AddSource(result.FileName, result.ToString());
                }
            );
        }
    }
}