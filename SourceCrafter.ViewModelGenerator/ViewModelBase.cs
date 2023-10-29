using System;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Linq;

namespace SourceCrafter.Mvvm
{
    public abstract class ViewModelBase : IObservable
    {
        public event PropertyChangedEventHandler? PropertyChanged;
        public event PropertyChangingEventHandler? PropertyChanging;

        protected void OnPropertyChanged(PropertyChangedEventArgs propertyNameEvtArg) => PropertyChanged?.Invoke(this, propertyNameEvtArg);

        protected void OnPropertyChanging(PropertyChangingEventArgs propertyNameEvtArg) => PropertyChanging?.Invoke(this, propertyNameEvtArg);

        void IObservable.RaisePropertyChange(PropertyChangedEventArgs args) => OnPropertyChanged(args);

        void IObservable.RaisePropertyChanging(PropertyChangingEventArgs args) => OnPropertyChanging(args);

        void IObservable.Subscribe(PropertyChangedEventHandler handler) => Subscribe(handler);
        
        protected void Subscribe(PropertyChangedEventHandler handler)
        {
            if (PropertyChanged?.GetInvocationList().Contains(handler) ?? false)
                return;
            PropertyChanged += handler;
        }

        public void NotifyExternalDependency(ViewModelBase external, string propertyName, string externalPropertyName) =>
            Subscribe((s, a) => {
                if (a.PropertyName == propertyName)
                    external.OnPropertyChanged(new(externalPropertyName));
            });
    }
}
