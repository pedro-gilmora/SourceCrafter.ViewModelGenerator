using System;
using System.Net.Mime;


namespace SourceCrafter.Mvvm.Attributes;

[AttributeUsage(AttributeTargets.Interface)]
public sealed class ReactiveAttribute : Attribute {}

[AttributeUsage(AttributeTargets.Method)]
public sealed class CommandAttribute : Attribute {}