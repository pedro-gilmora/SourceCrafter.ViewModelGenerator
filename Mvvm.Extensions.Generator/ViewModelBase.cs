using System;
using System.ComponentModel;

namespace Mvvm.Extensions.Generator
{
    public abstract class ViewModelBase: INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler? PropertyChanged;

        protected void OnPropertyChanged(PropertyChangedEventArgs propertyNameEvtArg)
        {
            PropertyChanged?.Invoke(this, propertyNameEvtArg);
        }
    }


}
