using System;
using Microsoft.Extensions.DependencyInjection;

namespace SourceCrafter.Mvvm
{
    internal interface IValidatable 
    { 
        abstract bool ValidateRules(IServiceProvider serviceProvider);
    }
}