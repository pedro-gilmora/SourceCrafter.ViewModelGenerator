using CommunityToolkit.Mvvm.Input;
using Mvvm.Extensions.Generator.Attributes;

namespace Mvvm.Extensions.UnitTests
{
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
    //    string ActionName => $"Action is called: {Action.Name}";
    //    IAction Action { get; set; }
    //[Ignore]
    //    string? LastName { get; set; }
        //string FirstName { get; set; }
        //string Name => $"{FirstName} {LastName}".Trim();
        //bool Is18 { get => Age == 18; set => Age = value ? 18 : Age; }
        //int Age { get; set; }
        //bool CanDrink { get; set; }
        //bool IsUnder18
        //{
        //    get => Age >= 18;
        //    set
        //    {
        //        Age = value ? 18 : 17;
        //        if (IsUnder18) CanDrink = true; 
        //        else CanDrink = false;
        //    }
        //}
        //AsyncRelayCommand<Role> SaveCommand { get; }
    }

    [ObservableModel]
    public interface IAction
    {
        string Name { get; set; }
    }

    public partial class User
    {
        //private partial bool CanExecuteSave(Role parameter) => true;

        //private partial Task ExecuteSaveAsync(Role parameter) => Task.CompletedTask;
    }
}
