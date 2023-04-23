using System;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Linq;

namespace Mvvm.Extensions.Generator
{
    public abstract class ViewModelBase: IObservable
    {
        public event PropertyChangedEventHandler? PropertyChanged;

        void IObservable.Subscribe(PropertyChangedEventHandler handler) {
            if (PropertyChanged?.GetInvocationList().Contains(handler) ?? false)
                return;
            PropertyChanged += handler;
        }

        protected void OnPropertyChanged(PropertyChangedEventArgs propertyNameEvtArg)
        {
            PropertyChanged?.Invoke(this, propertyNameEvtArg);
        }

        void IObservable.RaisePropertyChange(PropertyChangedEventArgs handler)
        {
            OnPropertyChanged(handler);
        }

        public void NotifyExternalDependency(IObservable external, string propertyName, string externalPropertyName)
        {
            ((IObservable)this).Subscribe((s, a) =>
            {
                if (a.PropertyName == propertyName)
                    external.RaisePropertyChange(new(externalPropertyName));
            });
        }
    }


}
