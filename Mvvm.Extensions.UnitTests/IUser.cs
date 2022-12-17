using CommunityToolkit.Mvvm.Input;
using Mvvm.Extensions.Generator.Attributes;
using System.Collections.ObjectModel;
using System.Collections.Specialized;

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
        string FirstName { get; set; }
        [Ignore]
        string? LastName { get; set; }
        string Name => $"{FirstName} {LastName}".Trim();
        int Age { get; set; }
        bool CanDrink { get; set; }
        bool IsUnder18
        {
            get => Age >= 18;
            set
            {
                Age = value ? 18 : 17;
                if (IsUnder18) CanDrink = true; else CanDrink = false;
            }
        }
        AsyncRelayCommand<Role> AddParentCommand { get; }
    }

    public partial class User
    {
        private partial bool CanExecuteAddParent(Role parameter) => true;

        private partial Task ExecuteAddParentAsync(Role parameter) => Task.CompletedTask;
    }
}
