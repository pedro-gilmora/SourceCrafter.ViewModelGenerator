# ✨ **SourceCrafter.ViewModelGenerator**: Simple partial classes for observable view models 

### Given the following spec interface
```csharp
public enum Role
{
    Admin,
    Moderator,
    Guest,
    User
}

[ObservableModel]
public interface IUser
{
    string FirstName { get; set; }
    [Ignore]
    string? LastName { get; set; }
    int Age { get; set; }
    bool CanDrink { get; set; }
    string Name => $"{FirstName} {LastName}".Trim();
    bool IsUnder18
    {
        get => Age >= 18;
        set
        {
            Age = value ? 18 : 17;
            CanDrink = !IsUnder18;
        }
    }
    AsyncRelayCommand<Role> AddParentCommand { get; }
}
```

should generates the following view model class (based on the previous definition example):

```csharp
public partial class User : ViewModelBase, IUser 
{
    private string 
        _firstName;
    private int 
        _age;
    private bool 
        _canDrink;
    private AsyncRelayCommand<Role> 
        _addParentCommand;

    private static readonly PropertyChangedEventArgs
        _nameChangedEvtArg = new("Name"),
        _isUnder18ChangedEvtArg = new("IsUnder18"),
        _firstNameChangedEvtArg = new("FirstName"),
        _ageChangedEvtArg = new("Age"),
        _canDrinkChangedEvtArg = new("CanDrink");

    public string Name => $"{FirstName} {LastName}".Trim();

    public bool IsUnder18 { 
        get => Age >= 18;
        set {
            Age = value ? 18 : 17;
            if (IsUnder18) CanDrink = true; else CanDrink = false;
            OnPropertyChanged(_isUnder18ChangedEvtArg);
        }
    }

    public string FirstName
    {
        get => _firstName;
        set {
            if(value == _firstName) 
                return;
            _firstName = value;
            OnPropertyChanged(_firstNameChangedEvtArg);
            OnPropertyChanged(_nameChangedEvtArg);
        }
    }

    public string? LastName { get; set; }

    public int Age
    {
        get => _age;
        set {
            if(value == _age) 
                return;
            _age = value;
            OnPropertyChanged(_ageChangedEvtArg);
            OnPropertyChanged(_isUnder18ChangedEvtArg);
        }
    }

    public bool CanDrink
    {
        get => _canDrink;
        set {
            if(value == _canDrink) 
                return;
            _canDrink = value;
            OnPropertyChanged(_canDrinkChangedEvtArg);
            OnPropertyChanged(_isUnder18ChangedEvtArg);
        }
    }

    public AsyncRelayCommand<Role> AddParentCommand => _addParentCommand ??= new AsyncRelayCommand<Role>(ExecuteAddParentAsync, CanExecuteAddParent, AsyncRelayCommandOptions.None);

    private partial bool CanExecuteAddParent(Role parameter);

    private partial Task ExecuteAddParentAsync(Role parameter);
}
```

And you will have to complement it with commands methods definition

```cs
public partial class User
{
    private partial bool CanExecuteAddParent(Role parameter) => true;

    private partial Task ExecuteAddParentAsync(Role parameter) => Task.CompletedTask;
}
```

## CommandOptionsAtribute

| Parameter | Type | Description |
| --------- | ---- | ----------- |
| **`generateCanExecute`** | `bool` | Controls whether the `CanExecute` method will be generated or not. Default is `true` |
| **`asyncOptions`** | [`AsyncRelayCommandOptions`](https://github.com/CommunityToolkit/dotnet/blob/e8969781afe537ea41a964a15b4ccfee32e095df/src/CommunityToolkit.Mvvm/Input/AsyncRelayCommandOptions.cs) | Options to customize the behavior of [`AsyncRelayCommand`](https://github.com/CommunityToolkit/dotnet/blob/e8969781afe537ea41a964a15b4ccfee32e095df/src/CommunityToolkit.Mvvm/Input/AsyncRelayCommand.cs) and [`AsyncRelayCommand`](https://github.com/CommunityToolkit/dotnet/blob/e8969781afe537ea41a964a15b4ccfee32e095df/src/CommunityToolkit.Mvvm/Input/AsyncRelayCommand%7BT%7D.cs) instances. Defaults to `AsyncRelayCommandOptions.None`