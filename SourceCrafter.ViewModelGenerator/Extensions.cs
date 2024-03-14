#nullable enable
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices.ComTypes;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SourceCrafter.Mvvm;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace SourceCrafter;

public static class Helpers
{

    private static bool IgnoreProperty(IPropertySymbol property) =>
        property
            .GetAttributes()
            .Any(attr => attr.AttributeClass?.Name is "IgnoreAttribute" or "Ignore");

    public static void Walk(this IEnumerable enumrbl)
    {
        var enumerator = enumrbl.GetEnumerator();
        while (enumerator.MoveNext());
    }
    public static bool Walk(this IEnumerable enumrbl, bool ret)
    {
        var enumerator = enumrbl.GetEnumerator();
        while (enumerator.MoveNext());
        return ret;
    }

    public static bool TryPeek<T>(this Stack<T> values, out T _out) {
        _out = default!;
        if(values.Count > 0)
        {
            _out = values.Peek();
            return true;
        }
        return false;
    }

    public static bool HasAttribute(this ISymbol property, string namespce, string attrName)
        => property.GetAttributes().Any(attr => true == 
            attr.AttributeClass?
                .ToGlobalNamespace()
                .StartsWith(string.Format("global::{0}.{1}", namespce, attrName)));
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