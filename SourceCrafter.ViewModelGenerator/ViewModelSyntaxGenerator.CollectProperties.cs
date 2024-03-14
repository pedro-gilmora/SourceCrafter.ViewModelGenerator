using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Runtime.CompilerServices;
using System.Buffers;
using System.ComponentModel;
using SourceCrafter.Mvvm;
using System.Linq.Expressions;
using System.Reflection;

namespace SourceCrafter.Mvvm;

internal sealed partial class ViewModelSyntaxGenerator
{
    static readonly FieldInfo _underlyingField = Type.GetType("Microsoft.CodeAnalysis.CSharp.Symbols.PublicModel.PropertySymbol, Microsoft.CodeAnalysis.CSharp, Culture=neutral, PublicKeyToken=31bf3856ad364e35").GetField("_underlying", BindingFlags.Instance | BindingFlags.NonPublic);
    static readonly MethodInfo isAutoPropertyGetter = Type.GetType("Microsoft.CodeAnalysis.CSharp.Symbols.SourcePropertySymbol, Microsoft.CodeAnalysis.CSharp, Culture=neutral, PublicKeyToken=31bf3856ad364e35").GetProperty("IsAutoProperty", BindingFlags.Instance | BindingFlags.NonPublic).GetMethod;
    private void CollectPropertyInfo(IPropertySymbol propInfo)
    {
        string
            propName = propInfo.Name,
            typeName = propInfo.Type.ToGlobalNamespace(),
            fieldName = GetFieldName(propName);

        var info = PropertySyntaxInfo.Create(propInfo, typeName, propName, fieldName, (bool)isAutoPropertyGetter.Invoke(_underlyingField.GetValue(propInfo), []));

        if (!info.Ignore && info.Getter != null)
        {
            Dictionary<SyntaxNode, HashSet<PropertyDependencyTree>> returnedSymbolsByVars = [];

            info.Getter.DescendantNodes(node => WalkDownNodes(_dependencies, node, new())).Walk();

            bool WalkDownNodes(PropertyDependencyTree currentNestedScope, SyntaxNode node, (SyntaxNode, HashSet<PropertyDependencyTree>)? variableCollector = null)
            {
                switch (node)
                {
                    case AssignmentExpressionSyntax
                    {
                        Left: DeclarationExpressionSyntax { 
                            Designation: ParenthesizedVariableDesignationSyntax { 
                                Variables: { Count: > 0 and int count } vars } },
                        Right: TupleExpressionSyntax { Arguments: { } args } tupleRight
                    } declarator
                    :
                        for (int i = 0; i < count; i++)
                            RegisterVariable(currentNestedScope, vars[i], args[i]);
                        return false;
                    case VariableDesignationSyntax { Parent: RecursivePatternSyntax or DeclarationPatternSyntax } vd
                    :
                        returnedSymbolsByVars[vd] = [currentNestedScope];
                        break;
                    case VariableDeclaratorSyntax declarator
                    :
                        if (declarator.Initializer is { Value: { } value })
                            RegisterVariable(currentNestedScope, declarator, value);
                        return false;
                    case IsPatternExpressionSyntax isPattern
                    :
                        //currentScope.vars.Add(declarator.Initializer, new());
                        return ReadChainedMembers(isPattern.Expression, out var lastNode)
                            && WalkDownNodes(lastNode, isPattern.Pattern, variableCollector) && false;
                    case SubpatternSyntax subpattern
                    :
                        return ReadChainedMembers(subpattern.ExpressionColon?.Expression ?? subpattern.NameColon?.Name!, out var lastNode1)
                            && WalkDownNodes(lastNode1, subpattern.Pattern, variableCollector) && false;
                    case RecursivePatternSyntax recursivePattern
                    :
                        if(recursivePattern.PropertyPatternClause is { } propertyClause)
                            propertyClause.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();

                        else if (recursivePattern.PositionalPatternClause is { Subpatterns: { } positionalClauseItems })
                            foreach (var subPattern in positionalClauseItems)
                                subPattern.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();

                        return false;
                    case BinaryPatternSyntax binaryPattern
                    :
                        binaryPattern.Left.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();
                        binaryPattern.Right.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();

                        return false;
                    case BinaryExpressionSyntax binaryExpr
                    :
                        binaryExpr.Left.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();
                        binaryExpr.Right.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();
                        return false;
                    case InterpolatedStringExpressionSyntax { Contents: { Count: > 0 } contents }
                    :
                        foreach (var item in contents)
                        {
                            if (item is InterpolationSyntax { Expression: { } expr })
                                expr.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();
                        }
                        return false;
                    case InvocationExpressionSyntax { ArgumentList.Arguments: { } args, Expression: { } expr }
                    :
                        switch (expr)
                        {
                            case MemberAccessExpressionSyntax { Expression: { } subExpr }:
                                subExpr.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();
                                break;
                            case ConditionalAccessExpressionSyntax { Expression: { } subExpr }:
                                subExpr.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();
                                break;
                            default:
                                expr.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();
                                break;
                        }

                        foreach (var item in args)
                            item.Expression.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, variableCollector)).Walk();

                        return false;
                    case MemberAccessExpressionSyntax or ConditionalAccessExpressionSyntax
                    :
                        return ReadChainedMembers(node, out var lastNode2) && false;
                    case IdentifierNameSyntax { IsVar: false } id
                    :
                        if (IsParentInvocation(node, info.Getter) || !IsNotifiableContainingType(_model, id, out var prop, out var type))
                            return true;

                        var newNode = currentNestedScope.AddDependencies(true, propName, prop);

                        if (variableCollector is ({ } n, { } ch))
                            ch.Add(newNode);

                        return false;
                }

                return true;

                bool ReadChainedMembers(SyntaxNode node, out PropertyDependencyTree lastNode)
                {
                    int i = -1;
                    SyntaxNode? varRef = default;
                    IdentifierNameSyntax? lastId = default;
                    PropertyDependencyTree? _lastNode = lastNode = default!;
                    var exit = false;
                    int parentCount = 0;
                    List<PropertyDependencyTree> parents = [];

                    node.DescendantNodes(subNode =>
                    {
                        if (exit || subNode is not IdentifierNameSyntax id)
                            return true;

                        var symbolInfo = _model.GetSymbolInfo(id);

                        i++;

                        if (IsParentInvocation(id, info.Getter))
                        {
                            return !(exit = true);
                        }
                        else if (ReturnsNotifiableType(symbolInfo.Symbol, out var propType))
                        {
                            varRef = symbolInfo.Symbol!.DeclaringSyntaxReferences.LastOrDefault()?.GetSyntax();
                            return exit = varRef == null;
                        }
                        else if (IsNotifiable(symbolInfo, out var prop, out var type))
                        {
                            lastId = id;

                            if (varRef != null && returnedSymbolsByVars.TryGetValue(varRef, out var _parents))
                            {
                                foreach (var item in _parents)
                                {
                                    parents.Add(item.AddDependencies(true, propName, prop));
                                    parentCount++;
                                }
                            }
                            else if (parentCount > 0)
                            {
                                for (int i = 0; i < parentCount; i++)
                                {
                                    parents[i] = parents[i].AddDependencies(true, propName, prop);
                                }
                            }
                            else
                            {
                                _lastNode = (_lastNode ?? currentNestedScope).AddDependencies(true, propName, prop);
                            }
                            //For multiple output variable (patterns, ternary and switch expressions)
                            //if (currentAssigning != null && currentScope.vars.TryGetValue(currentAssigning, out var parent))
                            //    foreach (var item in parent)
                            //        _lastNode = new Node(prop, item);
                            //else

                            return true;
                        }

                        return exit = lastId?.Span.End < node.Span.End;

                    }).Walk();

                    if (_lastNode != null)
                    {
                        if (variableCollector is ({ } n, { } ch))
                            ch.Add(_lastNode);
                        lastNode = _lastNode;
                    }

                    return !exit;
                }

                void RegisterVariable(PropertyDependencyTree currentNestedScope, SyntaxNode lhs, SyntaxNode rhs)
                {
                    HashSet<PropertyDependencyTree> returned = [];

                    rhs.DescendantNodes(n => WalkDownNodes(currentNestedScope, n, (rhs, returned))).Walk();

                    if (returned.Count > 0)
                        returnedSymbolsByVars[rhs] = returned;
                }
            }
        }

        if (info.IsReactive)
            _buildProperties += () => BuildProperty(info);
    }
}

internal class TypeFields(string typeName, Action<int> builders)
{
    internal string TypeName => typeName;
    internal Action<int> Builders = builders;
}