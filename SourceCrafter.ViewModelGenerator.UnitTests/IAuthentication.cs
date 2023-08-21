using CommunityToolkit.Mvvm.Input;
using SourceCrafter.Mvvm.Attributes;

namespace FacilCuba.ViewModels
{
    [Reactive]
    public interface IAuthentication
    {
        string? Email { get; set; }
        string? Password { get; set; }
        string? Token { get; set; }
        bool IsBusy { get; set; }
        bool ClearBrowserData { get; }
        bool CanLogin => !IsBusy && !string.IsNullOrEmpty(Email?.Trim()) && !string.IsNullOrEmpty(Password?.Trim());

        [CommandOptions(false)]
        AsyncRelayCommand LoginCommand { get; }
        [CommandOptions(false)]
        AsyncRelayCommand LogoutCommand { get; }
    }
}