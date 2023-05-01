using System;
using Microsoft.Extensions.DependencyInjection;

namespace SourceCrafter
{
    internal interface IValidatable 
    { 
        abstract bool ValidateRules(IServiceProvider serviceProvider);
    }
}