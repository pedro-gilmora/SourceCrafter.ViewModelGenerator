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

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        protected void Set<T>(ref T value, T newValue, [CallerMemberName] string propName = null!) where T : class
        {
            if (value != newValue) return;

            PropertyChanging?.Invoke(this, new(propName));

            value = newValue;

            PropertyChanged?.Invoke(this, new(propName));
        }

        string? lastChangedProp = null;
        protected virtual void OnPropertyChanged(PropertyChangedEventArgs propertyNameEvtArg) 
        {
            if (lastChangedProp == propertyNameEvtArg.PropertyName) return;
            lastChangedProp = propertyNameEvtArg.PropertyName;
            PropertyChanged?.Invoke(this, propertyNameEvtArg);
            lastChangedProp = null;
        }
        protected virtual void OnPropertyChanging(PropertyChangingEventArgs propertyNameEvtArg) => PropertyChanging?.Invoke(this, propertyNameEvtArg);

        void IObservable.RaisePropertyChange(PropertyChangedEventArgs args) => OnPropertyChanged(args);

        void IObservable.RaisePropertyChanging(PropertyChangingEventArgs args) => OnPropertyChanging(args);

        void IObservable.Subscribe(PropertyChangedEventHandler handler) => Subscribe(handler);
        
        protected void Subscribe(PropertyChangedEventHandler handler)
        {
            PropertyChanged -= handler;
            PropertyChanged += handler;
        }
    }
}
