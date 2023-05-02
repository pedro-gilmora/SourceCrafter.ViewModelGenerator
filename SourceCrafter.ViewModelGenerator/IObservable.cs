using System.ComponentModel;

namespace SourceCrafter.Mvvm
{
    public interface IObservable : INotifyPropertyChanged
    {
        void Subscribe(PropertyChangedEventHandler handler);
        void RaisePropertyChange(PropertyChangedEventArgs handler);
    }
}