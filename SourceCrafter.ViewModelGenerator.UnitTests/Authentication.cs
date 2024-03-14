using CommunityToolkit.Mvvm.Input;

using SourceCrafter.Mvvm;
using SourceCrafter.Mvvm.Attributes;

namespace FacilCuba.ViewModels
{
    [Reactive]
    public abstract partial class Authentication : ViewModelBase
    {
        public virtual string? Email { get; set; }
        public virtual string? Password { get; set; }
        public virtual string? Token { get; set; }
        public virtual bool IsBusy { get; set; }
        public bool ClearBrowserData { get; }
        public virtual bool CanLogin => !IsBusy && !string.IsNullOrEmpty(Email?.Trim()) && !string.IsNullOrEmpty(Password?.Trim());
    }
}