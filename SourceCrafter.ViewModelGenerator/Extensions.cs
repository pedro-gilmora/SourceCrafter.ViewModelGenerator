#nullable enable
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace SourceCrafter;

public static class Helpers
{
    public static void Deconstruct(
        this IPropertySymbol propSymbol,
        out bool isImplemented,
        out CSharpSyntaxNode? getter,
        out CSharpSyntaxNode? setter,
        out bool isCommand,
        out bool isReadOnly,
        out bool ignore)
    {
        isCommand = propSymbol.Type.Name.Contains("RelayCommand") && propSymbol.Name.EndsWith("Command");
        (isImplemented, isReadOnly, getter, setter, ignore) = propSymbol switch
        {
            { IsIndexer: false, DeclaringSyntaxReferences: [.., { } propSyntaxRef] } when propSyntaxRef.GetSyntax() is PropertyDeclarationSyntax { ExpressionBody: { } body } =>
                (true, true, body, null, IgnoreProperty(propSymbol)),
            { IsIndexer: false, GetMethod: var getMethod, SetMethod: var setMethod, IsReadOnly: var isRo } =>
                (!(getMethod?.IsAbstract ?? true),
                    isRo,
                    TryReduceMethod(getMethod),
                    TryReduceMethod(setMethod),
                    IgnoreProperty(propSymbol)),
            _ => default
        };
    }

    private static bool IgnoreProperty(IPropertySymbol property) =>
        property
            .GetAttributes()
            .Any(attr => attr.AttributeClass?.Name is "IgnoreAttribute" or "Ignore");

    public static bool HasAttribute(this ISymbol property, string namespce, string attrName)
        => property.GetAttributes().Any(attr =>
            attr.AttributeClass is { Name: { } name, ContainingNamespace: { } cNamespace } &&
                            cNamespace.ToString() == namespce &&
                            name.StartsWith(attrName));
    public static bool ContainsAttribute(this IPropertySymbol property, string attrName) =>
        property.GetAttributes().Any(attr => attr.AttributeClass?.Name?.StartsWith(attrName) ?? false);

    private const int
        SetAccessorDeclaration = (int)SyntaxKind.SetAccessorDeclaration;

    private static CSharpSyntaxNode? TryReduceMethod(IMethodSymbol? method) => (method?.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax() as AccessorDeclarationSyntax) switch
    {
        { RawKind: { } kind, ExpressionBody: { } retValue } =>
            kind == SetAccessorDeclaration
                ? ExpressionStatement(retValue.Expression)
                : retValue,
        { RawKind: { } kind, Body.Statements: [{ } retValue] } =>
            retValue,
        var ret =>
            ret?.Body
    };

}