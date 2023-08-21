using SourceCrafter.Mvvm.Attributes;

namespace FacilCuba.ViewModels
{
    [Reactive]
    public interface IView
    {
        string Icon { get; set; }
        string Title { get; set; }
        object Content { get; set; }
    }
}