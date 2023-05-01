using Mvvm.Extensions.Generator.Attributes;

namespace FacilCuba.ViewModels
{
    [ObservableModel]
    public interface IView
    {
        string Icon { get; set; }
        string Title { get; set; }
        object Content { get; set; }
    }
}