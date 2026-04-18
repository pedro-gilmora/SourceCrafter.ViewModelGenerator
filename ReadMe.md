# ✨ **SourceCrafter.ViewModelGenerator**: Simple partial classes for observable view models 

### Given the following spec interface
```csharp
[Reactive]
public partial class AppManager
{
    public partial User? User { get; set; }

    public partial Authentication? Authentication { get; set; }

    public bool IsAuthenticated => Authentication is { Token.Length: 0 } or { CanLogin: false };
}
```

should generates the following view model class (based on the previous definition example):

```csharp
//<auto generated>
#nullable enable

namespace FacilCuba.ViewModels;

public partial class AppManager : global::SourceCrafter.Mvvm.ViewModelBase
{    
    public partial global::SourceCrafter.ViewModel.UnitTests.User? User 
    {
        get;
        set 
        {
            if(Equals(value, field))
                return;
            field = value;
            OnPropertyChanged(new("User"));
        }
    }
    
    public partial global::FacilCuba.ViewModels.Authentication? Authentication 
    {
        get;
        set 
        {
            if(Equals(value, field))
                return;
            field = value;
            NotifyChange(new("Authentication"));
        }
    }

    protected void NotifyChange(global::System.ComponentModel.PropertyChangedEventArgs evtArgs) 
    {
        OnPropertyChanged(evtArgs);
        switch(evtArgs.PropertyName)
        {
            case "Authentication":
                (Authentication as global::SourceCrafter.Mvvm.IObservable)?.Subscribe((s0, e0) => 
                {
                    switch(e0.PropertyName)
                    {
                        case "Token":
                            OnPropertyChanged(new("Authentication"));
                            OnPropertyChanged(new("IsAuthenticated"));
                        break;
                        case "CanLogin":
                            OnPropertyChanged(new("Authentication"));
                            OnPropertyChanged(new("IsAuthenticated"));
                        break;
                    }
                });
                OnPropertyChanged(new("IsAuthenticated"));
            break;
        }
    }
}
```
