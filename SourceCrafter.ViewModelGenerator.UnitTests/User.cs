using CommunityToolkit.Mvvm.Input;

using SourceCrafter.Mvvm;
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

    [Reactive]
    public abstract partial class User : ViewModelBase
    {
        public string ActionName => $"Running action: {Action.Name}";
        public virtual IAction Action { get; set; }
        public virtual string FirstName { get; set; }
        public virtual string? LastName { get; set; }
        public string Name => $"{FirstName} {LastName}".Trim();
        public virtual bool Is18 { get => Age == 18; set => Age = value ? 18 : Age; }
        public virtual int Age { get; set; }
        public virtual bool CanDrink { get; set; }
        public virtual bool IsUnder18
        {
            get => Age < 18;
            set
            {
                Age = (value && Age >= 18) ? 17 : Age;

                if (IsUnder18) 
                    CanDrink = false; 
                else 
                    CanDrink = true;
            }
        }
    }

    public interface IAction
    {
        string? Name { get; set; }
    }

    public partial class User
    {
        public Task AddAsync(Role role) => Task.CompletedTask;
    }
}
