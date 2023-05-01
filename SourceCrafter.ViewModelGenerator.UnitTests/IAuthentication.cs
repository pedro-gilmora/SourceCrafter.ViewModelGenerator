using CommunityToolkit.Mvvm.Input;
//using FacilCuba.Infrastructure.QvaPay;
//using FacilCuba.Views;
using HttPie.Generator;
using Microsoft.Extensions.DependencyInjection;
using Mvvm.Extensions.Generator.Attributes;
using System.ComponentModel;
using System.Text.Json;

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