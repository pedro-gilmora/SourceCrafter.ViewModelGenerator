using System.ComponentModel;

namespace Mvvm.Extensions.Generator
{
    public interface IObservable : INotifyPropertyChanged
    {
        void Subscribe(PropertyChangedEventHandler handler);
        void RaisePropertyChange(PropertyChangedEventArgs handler);
    }
}