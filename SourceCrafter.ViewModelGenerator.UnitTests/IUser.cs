using CommunityToolkit.Mvvm.Input;
using SourceCrafter.Mvvm.Attributes;

namespace SourceCrafter.ViewModel.UnitTests
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
        string ActionName => $"Running action: {Action.Name}";
        IAction Action { get; set; }
        string FirstName { get; set; }
        string? LastName { get; set; }
        string Name => $"{FirstName} {LastName}".Trim();
        bool Is18 { get => Age == 18; set => Age = value ? 18 : Age; }
        int Age { get; set; }
        bool CanDrink { get; set; }
        bool IsUnder18
        {
            get => Age < 18;
            set
            {
                Age = value && Age >= 18 ? 17 : Age;
                if (IsUnder18) CanDrink = true;
                else CanDrink = false;
            }
        }
        RelayCommand<Role> SaveCommand { get; }
    }

    [ObservableModel]
    public interface IAction
    {
        string? Name { get; set; }
    }

    public partial class User
    {
        private partial bool CanExecuteSave(Role parameter) => true;

        partial void ExecuteSave(Role parameter)
        {
        }
    }
}
