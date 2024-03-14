
#if NETSTANDARD2_0 || NETSTANDARD2_1 || NETCOREAPP2_0 || NETCOREAPP2_1 || NETCOREAPP2_2 || NETCOREAPP3_0 || NETCOREAPP3_1 || NET45 || NET451 || NET452 || NET6 || NET461 || NET462 || NET47 || NET471 || NET472 || NET48


// ReSharper disable once CheckNamespace
using Microsoft.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;

namespace System.Runtime.CompilerServices
{
    /// <summary>
    /// Reserved to be used by the compiler for tracking metadata.
    /// This class should not be used by developers in source code.
    /// </summary>
    [EditorBrowsable(EditorBrowsableState.Never)]
    internal static class IsExternalInit
    {
    }
}

#endif

namespace SourceCrafter.Mvvm
{
    public static class RoslynExtensions
    {
        private readonly static SymbolDisplayFormat
            _globalizedNamespace = new(
                globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
                typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
                genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters | SymbolDisplayGenericsOptions.IncludeVariance,
                miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes | SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier),
            _globalizedNonGenericNamespace = new(
                globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
                typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
                miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes),
            _symbolNameOnly = new(typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameOnly),
            _typeNameFormat = new(
                typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypes,
                genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters | SymbolDisplayGenericsOptions.IncludeVariance,
                miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes | SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier);

        internal static string ToNameOnly(this ISymbol t) => t.ToDisplayString(_symbolNameOnly);

        internal static string ToGlobalNamespace(this ISymbol t) => t.ToDisplayString(_globalizedNamespace);

        internal static string ToGlobalNonGenericNamespace(this ISymbol t) => t.ToDisplayString(_globalizedNonGenericNamespace);

        internal static string ToTypeNameFormat(this ITypeSymbol t) => t.ToDisplayString(_typeNameFormat);

        public static bool IsNullable(this ITypeSymbol typeSymbol)
        {
            if (typeSymbol != null && (typeSymbol.NullableAnnotation == NullableAnnotation.Annotated || (typeSymbol is INamedTypeSymbol && typeSymbol.Name == "Nullable")))
            {
                return true;
            }

            return false;
        }

        public static bool AllowsNull(this ITypeSymbol typeSymbol)
        {
            if (typeSymbol == null || typeSymbol.IsValueType || typeSymbol.IsTupleType)
            {
                return typeSymbol?.IsNullable() ?? false;
            }

            return typeSymbol.IsReferenceType;
        }

        public static bool IsAttributeName(this string clsName, AttributeData attr)
        {
            return attr.HasAttribute(clsName);
        }

        public static bool HasAttribute(this AttributeData attr, string clsName)
        {
            return attr.AttributeClass?.Name == clsName;
        }
        public static void Deconstruct<TKey, TValue>(this KeyValuePair<TKey, TValue> source, out TKey key, out TValue val)
        {
            key = source.Key;
            val = source.Value;
        }

        public static string Join<T>(this IEnumerable<T> strs, Func<T, string> formmater, string? separator = "")
        {
            return string.Join(separator, strs.Select(formmater));
        }

        public static string Join<T>(this IEnumerable<T> strs, string? separator = "")
        {
            return strs.Join(t => t?.ToString() ?? "", separator);
        }

    }
}