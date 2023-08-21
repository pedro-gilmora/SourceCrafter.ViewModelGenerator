//using CommunityToolkit.Maui.Views;
using CommunityToolkit.Mvvm.DependencyInjection;
using SourceCrafter.Mvvm.Attributes;
using SourceCrafter.ViewModel.UnitTests;
using System.ComponentModel;

namespace FacilCuba.ViewModels
{
    [Reactive]
    public interface IAppManager
    {
        IUser? User { get; set; }

        IAuthentication Authentication { get; set; }

        bool IsAuthenticated => Authentication is { Token.Length: > 0, Email: { Length: 0} };
    }

    public partial class AppManager
    {
        public AppManager() 
        {
            //Authentication = ServiceProvider.GetService<Authentication>()!;
            //Authentication.PropertyChanged += OnPropertiesChanged;
        }

        private void OnPropertiesChanged(object? sender, PropertyChangedEventArgs e)
        {
            //if (e.PropertyName == nameof(Authentication.Token)) 
            //{
            //    OnPropertyChanged(_isAuthenticatedChangedEvtArg);
            //}
        }

        public static IServiceProvider ServiceProvider { get; internal set; } = null!;

        
    }
}