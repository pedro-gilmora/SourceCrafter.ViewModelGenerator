﻿#nullable enable
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace SourceCrafter;

[Generator]
internal class ViewModelGenerator : IIncrementalGenerator
{
    private static readonly object Lock = new();

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        context.RegisterSourceOutput(
            context.CompilationProvider,
            (sourceProducer, _) => sourceProducer.AddSource("SourceCrafter.Mvvm.Attributes.CommandOptionsAttribute.g.cs", @"using CommunityToolkit.Mvvm.Input;
using System;

namespace SourceCrafter.Mvvm.Attributes;

[AttributeUsage(AttributeTargets.Property, AllowMultiple = false)]
public class CommandOptionsAttribute : Attribute
{
    public CommandOptionsAttribute(bool canExecute, global::CommunityToolkit.Mvvm.Input.AsyncRelayCommandOptions asyncOption = global::CommunityToolkit.Mvvm.Input.AsyncRelayCommandOptions.None) { }
}"));
        context.RegisterSourceOutput(
            context.SyntaxProvider.ForAttributeWithMetadataName(
                $"{ViewModelSyntaxGenerator.NAMESPACE}.{ViewModelSyntaxGenerator.ATTRIBUTE}Attribute",
                static (n, _) => n is InterfaceDeclarationSyntax,
                static (ctx, _) => (Interface: (ITypeSymbol)ctx.TargetSymbol, Model: ctx.SemanticModel)
            ),
            static (sourceProducer, interfaceToGenerate) =>
            {
                try
                {
                    var result = new ViewModelSyntaxGenerator(interfaceToGenerate.Interface, interfaceToGenerate.Model);
                    sourceProducer.AddSource(result.FileName, result.ToString());
                }
                catch (System.Exception e)
                {
                    sourceProducer.AddSource(interfaceToGenerate.Interface.Name + ".error.cs", "/*" + e.ToString() + "*/");
                }
            }
        );
    }
}