//using CommunityToolkit.Maui.Views;
using CommunityToolkit.Mvvm.DependencyInjection;
using Mvvm.Extensions.Generator.Attributes;
using Mvvm.Extensions.UnitTests;
using System.ComponentModel;

namespace FacilCuba.ViewModels
{
    [ObservableModel]
    public interface IAppManager
    {
        IUser? User { get; set; }

        Authentication Authentication { get; set; }

        bool IsAuthenticated => Authentication is { Token: { } };
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