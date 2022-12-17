using System;
using Microsoft.Extensions.DependencyInjection;

namespace Mvvm.Extensions.Generator
{
    internal interface IValidatable 
    { 
        abstract bool ValidateRules(IServiceProvider serviceProvider);
    }
}