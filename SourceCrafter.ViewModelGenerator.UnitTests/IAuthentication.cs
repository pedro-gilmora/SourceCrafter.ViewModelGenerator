using CommunityToolkit.Mvvm.Input;
using SourceCrafter.Mvvm.Attributes;

namespace FacilCuba.ViewModels
{
    [ObservableModel]
    public interface IAuthentication
    {
        string? Email { get; set; }
        string? Password { get; set; }
        string? Token { get; set; }
        bool IsBusy { get; set; }
        bool CanLogin => !IsBusy && !string.IsNullOrEmpty(Email?.Trim()) && !string.IsNullOrEmpty(Password?.Trim());

        [CommandOptions(false)]
        AsyncRelayCommand LoginCommand { get; }
        [CommandOptions(false)]
        AsyncRelayCommand LogoutCommand { get; }
    }
}