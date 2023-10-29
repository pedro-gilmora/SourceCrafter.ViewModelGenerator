using System.ComponentModel;

namespace SourceCrafter.Mvvm
{
    public interface IObservable : INotifyPropertyChanged, INotifyPropertyChanging
    {
        void Subscribe(PropertyChangedEventHandler handler);
        void RaisePropertyChange(PropertyChangedEventArgs handler);
        void RaisePropertyChanging(PropertyChangingEventArgs handler);
    }
}