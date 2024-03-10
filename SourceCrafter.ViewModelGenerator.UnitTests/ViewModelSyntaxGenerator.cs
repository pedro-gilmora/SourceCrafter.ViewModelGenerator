using Microsoft.CodeAnalysis;

namespace SourceCrafter.ViewModelGenerator.UnitTests
{
    internal class ViewModelSyntaxGenerator
    {
        private INamedTypeSymbol namedTypeSymbol;
        private SemanticModel sm;

        public ViewModelSyntaxGenerator(INamedTypeSymbol namedTypeSymbol, SemanticModel sm)
        {
            this.namedTypeSymbol = namedTypeSymbol;
            this.sm = sm;
        }
    }
}