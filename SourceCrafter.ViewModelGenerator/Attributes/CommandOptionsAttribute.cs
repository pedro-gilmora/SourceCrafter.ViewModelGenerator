using CommunityToolkit.Mvvm.Input;
using System;

namespace SourceCrafter.Attributes;

[AttributeUsage(AttributeTargets.Property, AllowMultiple = false)]
public class CommandOptionsAttribute : Attribute
{
    public CommandOptionsAttribute(bool canExecute, AsyncRelayCommandOptions asyncOption = AsyncRelayCommandOptions.None) { }
}
