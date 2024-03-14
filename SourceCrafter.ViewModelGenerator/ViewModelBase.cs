using System;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;

namespace SourceCrafter.Mvvm
{
    public abstract class ViewModelBase : IObservable
    {
        public event PropertyChangedEventHandler? PropertyChanged;
        public event PropertyChangingEventHandler? PropertyChanging;

        protected void Set<T>(ref T value, T newValue, [CallerMemberName] string propName = null!) where T : class
        {
            if (value != newValue) return;

            PropertyChanging?.Invoke(this, new(propName));

            value = newValue;

            PropertyChanged?.Invoke(this, new(propName));
        }

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
    }
}
