using CommunityToolkit.Mvvm.Input;
using System;

namespace SourceCrafter.Mvvm.Attributes;

[AttributeUsage(AttributeTargets.Property, AllowMultiple = false)]
public class CommandOptionsAttribute : Attribute
{
    public CommandOptionsAttribute(bool canExecute, AsyncRelayCommandOptions asyncOption = AsyncRelayCommandOptions.None) { }
}
