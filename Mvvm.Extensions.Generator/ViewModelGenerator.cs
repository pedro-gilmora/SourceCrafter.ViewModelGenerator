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
            var interfaceDeclarations = context.SyntaxProvider
                .ForAttributeWithMetadataName(
                    "Mvvm.Extensions.Generator.Attributes.ObservableModelAttribute",
                    static (n, _) => n is InterfaceDeclarationSyntax,
                    static (ctx, _) => new { type = (ITypeSymbol)ctx.TargetSymbol, model = ctx.SemanticModel }
                );

            context.RegisterSourceOutput(
                interfaceDeclarations,
                static (sourceProducer, interfaceToGenerate) =>
                {
                    var result = new ViewModelGeneratorSyntax(interfaceToGenerate.type, interfaceToGenerate.model);
                    sourceProducer.AddSource(result.FileName, result.ToString());
                }
            );
        }
    }
}