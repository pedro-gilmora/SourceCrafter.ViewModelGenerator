using CommunityToolkit.Mvvm.Input;

using SourceCrafter.Mvvm;
using SourceCrafter.Mvvm.Attributes;

namespace FacilCuba.ViewModels
{
    [Reactive]
    public abstract partial class Authentication : ViewModelBase
    {
        public partial string? Email { get; set; }
        public partial string? Password { get; set; }
        public partial string? Token { get; set; }
        public partial bool IsBusy { get; set; }
        public bool ClearBrowserData { get; }
        public virtual bool CanLogin => !IsBusy && !string.IsNullOrEmpty(Email?.Trim()) && !string.IsNullOrEmpty(Password?.Trim());
    }
}