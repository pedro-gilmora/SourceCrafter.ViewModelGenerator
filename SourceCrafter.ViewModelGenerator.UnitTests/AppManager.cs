//using CommunityToolkit.Maui.Views;
using SourceCrafter.Mvvm;
using SourceCrafter.Mvvm.Attributes;
using SourceCrafter.ViewModel.UnitTests;

namespace FacilCuba.ViewModels
{
    [Reactive]
    public abstract partial class AppManager : ViewModelBase
    {
        public virtual User? User { get; set; }

        public virtual Authentication? Authentication { get; set; }

        public bool IsAuthenticated => Authentication is { Token.Length: 0 } or { CanLogin: false };
    }
}