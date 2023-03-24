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
        string FirstName { get; set; }
        [Ignore]
        string? LastName { get; set; }
        string Name => $"{FirstName} {LastName}".Trim();
        string AgeStatus => $"You're {(IsUnder18 ? "not " : "")}old enough";
        int Age { get; set; }
        bool IsUnder18
        {
            get => Age < 18;
            set => Age = IsUnder18 ? 17 : Age;
        }
        AsyncRelayCommand<Role> AddParentCommand { get; }
    }

    public partial class User
    {
        private partial bool CanExecuteAddParent(Role parameter) => true;

        private partial Task ExecuteAddParentAsync(Role parameter) => Task.CompletedTask;
    }
}
