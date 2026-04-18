using CommunityToolkit.Mvvm.Input;

using Newtonsoft.Json.Linq;

using SourceCrafter.Mvvm;
using SourceCrafter.Mvvm.Attributes;

using System.ComponentModel;

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
    public partial class User
    {
        public string ActionName => $"Running action: {Action.Name}";
        public partial IAction Action { get; set; }
        public partial string FirstName { get; set; }
        public partial string? LastName { get; set; }
        public string Name => $"{FirstName} {LastName}".Trim();
        public bool Is18 => Age == 18;
        public partial int Age { get; set; }
        public partial bool CanDrink { get; set; }

        protected override void OnPropertyChanged(PropertyChangedEventArgs propertyNameEvtArg)
        {
            base.OnPropertyChanged(propertyNameEvtArg);

            if (propertyNameEvtArg.PropertyName == nameof(Age))
            {
                Age = (CanDrink = Age >= 18) ? 17 : Age;
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
