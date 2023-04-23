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
using static Mvvm.Extensions.Generator.ViewModelGeneratorSyntax;

namespace Mvvm.Extensions.Generator
{
    public static class Helpers
    {
        public static void Deconstruct<TKey, TValue>(this KeyValuePair<TKey, TValue> source, out TKey key, out TValue val)
        {
            (key, val) = (source.Key, source.Value);
        }
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
            property.GetAttributes().Any(attr => attr.AttributeClass?.Name is "IgnoreAttribute" or "Ignore");

        public static bool HasAttribute(this ISymbol property, string namespce, string attrName) =>
            property.GetAttributes().Any(attr =>
            {
                return attr.AttributeClass is { Name: { } name, ContainingNamespace: { } cNamespace } &&
                                cNamespace.ToString() == namespce &&
                                name.StartsWith(attrName);
            });
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

        public static bool HasName(this string clsName, AttributeData attr) => attr.AttributeClass?.Name == clsName;
        public static string Join<T>(this IEnumerable<T> strs, Func<T, string> formmater, string? separator = "")
        {
            return string.Join(separator, strs.Select(formmater));
        }

        public static StringBuilder AppendIf(this StringBuilder builder, bool condition, Func<object?> getter)
        {
            if (condition)
                builder.Append(getter());
            return builder;
        }

        public static StringBuilder AppendFormatIf(this StringBuilder builder, bool condition, Func<StringFormatter, StringBuilder> ifAppend, Func<StringFormatter, StringBuilder>? elseAppend = null)
        {
            if (condition)
                return ifAppend(builder.AppendFormat);
            return elseAppend?.Invoke(builder.AppendFormat) ?? builder;
        }

        public static StringBuilder AppendLineIf(this StringBuilder builder, bool condition, Func<string?> getter)
        {
            if (condition)
                builder.AppendLine(getter());
            return builder;
        }
        public static StringBuilder InsertJoin<T>(this StringBuilder builder, int startIndex, IEnumerable<T> strs, Func<T, string?> formatter, string separator = "") =>
            strs.Select(formatter)
                .Aggregate(builder, (sb, s) => sb
                    .Insert(startIndex, separator)
                    .Insert(startIndex += separator.Length, (startIndex += s?.Length ?? 0, s).s));

        public static StringBuilder InsertJoin<T>(this StringBuilder builder, int startIndex, IEnumerable<T> strs, string separator = "") =>
            strs.Select(a => a?.ToString())
                .Aggregate(builder, (sb, s) => sb
                    .Insert(startIndex, separator)
                    .Insert(startIndex += separator.Length, (startIndex += s.Length, s).s));

        public static string Join<T>(this IEnumerable<T> strs, string? separator = "")
        {
            return strs.Join(t => t?.ToString() ?? "", separator);
        }

        public static void AddNested<TList, TKey, TValueItem>(this IDictionary<TKey, TList> listHash, TKey key, TValueItem valueItem)
            where TList : ICollection<TValueItem>, new()
        {
            if (listHash.TryGetValue(key, out var valueItems))
            {
                valueItems.Add(valueItem);
            }
            else
            {
                listHash.Add(key, new() { valueItem });
            }
        }

        public static T[] ArrayFrom<T>(params T[] items) => items;

        public static string ToCamelCase(this string name)
        {
            StringBuilder builder = new();
            var current = -1;
            var length = name.Length;
            var needUpper = true;
            var lastChar = char.MinValue;

            while (++current < length)
            {
                var ch = name[current];

                if (char.IsDigit(ch))
                {
                    if (builder.Length == 0)
                        builder.Append("_");

                    builder.Append(ch);
                    needUpper = true;
                }
                else if (char.IsSeparator(ch) || ch == '_' || ch == '-' || ch == '~')
                    needUpper = true;
                else if (builder.Length == 0 && char.IsUpper(ch))
                {
                    builder.Append(char.ToLowerInvariant(ch));
                    needUpper = false;
                }
                else if (builder.Length > 0 && (needUpper || char.IsLower(lastChar) && char.IsUpper(ch)))
                {
                    builder.Append(char.ToUpperInvariant(ch));
                    needUpper = false;
                }
                else
                {
                    builder.Append(ch);
                    needUpper = false;
                }

                lastChar = ch;
            }

            return builder.ToString();
        }

        public static string ToPascalCase(this string name)
        {
            StringBuilder builder = new();
            var current = -1;
            var length = name.Length;
            var needUpper = true;
            var lastChar = char.MinValue;

            while (++current < length)
            {
                var ch = name[current];

                if (char.IsDigit(ch))
                {
                    if (builder.Length == 0)
                        builder.Append("_");

                    builder.Append(ch);
                    needUpper = true;
                }
                else if (char.IsSeparator(ch) || ch == '_' || ch == '-' || ch == '~')
                    needUpper = true;

                else if (needUpper || char.IsLower(lastChar) && char.IsUpper(ch))
                {
                    builder.Append(char.ToUpperInvariant(ch));
                    needUpper = false;
                }
                else
                {
                    builder.Append(ch);
                    needUpper = false;
                }

                lastChar = ch;
            }

            return builder.ToString();
        }

        public static string ToLowerSnakeCase(this string name)
        {
            StringBuilder builder = new();
            int current = -1,
                length = name.Length;
            var start = false;
            var lastChar = char.MinValue;

            while (++current < length)
            {
                var ch = name[current];
                if (char.IsSeparator(ch))
                {
                    if (!start) continue;

                    builder.Append('_');
                }
                else
                {
                    var isUpper = char.IsUpper(ch);

                    if (char.IsLower(lastChar) && isUpper || char.IsDigit(ch))
                        builder.Append('_');

                    if (isUpper)
                        builder.Append(char.ToLowerInvariant(ch));
                    else
                        builder.Append(ch);

                    if (!start)
                        start = true;

                    lastChar = ch;
                }
            }

            return builder.ToString();
        }

        public static string ToUpperSnakeCase(this string name)
        {
            StringBuilder builder = new();
            var current = -1;
            var length = name.Length;
            var start = false;
            var lastChar = char.MinValue;

            while (++current < length)
            {
                var ch = name[current];
                if (char.IsSeparator(ch))
                {
                    if (!start) continue;

                    builder.Append('_');
                }
                else
                {
                    var isUpper = char.IsUpper(ch);

                    if (char.IsLower(lastChar) && isUpper || char.IsDigit(ch))
                        builder.Append('_');

                    if (!isUpper)
                        builder.Append(char.ToUpperInvariant(ch));
                    else
                        builder.Append(ch);

                    if (!start)
                        start = true;

                    lastChar = ch;
                }
            }

            return builder.ToString();
        }

        public static string ToCamelCase(this Enum value) => value.ToString().ToCamelCase();

        public static string ToPascalCase(this Enum value) => value.ToString().ToPascalCase();

        public static string ToLowerSnakeCase(this Enum value) => value.ToString().ToLowerSnakeCase();

        public static string ToUpperSnakeCase(this Enum value) => value.ToString().ToUpperSnakeCase();

    }

    [JetBrains.Annotations.StringFormatMethod("format")]
    public delegate StringBuilder StringFormatter(string format, params object?[] arguments);

}