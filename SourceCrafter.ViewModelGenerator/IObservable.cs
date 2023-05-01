using System.ComponentModel;

namespace SourceCrafter
{
    public interface IObservable : INotifyPropertyChanged
    {
        void Subscribe(PropertyChangedEventHandler handler);
        void RaisePropertyChange(PropertyChangedEventArgs handler);
    }
}