using Xunit;
using SourceCrafter.ViewModel.UnitTests.Implementation;
using FluentAssertions;

namespace SourceCrafter.ViewModelGenerator.UnitTests
{

    public static class Test
    {
        [Fact]
        public static void ShouldGenerateUser()
        {
            User r = new() { };
            int i = 0;
            r.PropertyChanged += (o, e) =>
            {
                switch (i++)
                {
                    case 0:
                        e.PropertyName.Should().Be(nameof(User.FirstName));
                        r.FirstName.Should().Be("Pedro");
                        break;
                    case 1:
                        e.PropertyName.Should().Be(nameof(User.Name));
                        r.Name.Should().Be("Pedro");
                        break;
                    case 2:
                        e.PropertyName.Should().Be(nameof(User.LastName));
                        r.LastName.Should().Be("Gil");
                        break;
                    case 3:
                        e.PropertyName.Should().Be(nameof(User.Name));
                        r.Name.Should().Be("Pedro Gil");
                        break;
                    case 4:
                        e.PropertyName.Should().Be(nameof(User.Age));
                        r.Age.Should().Be(17);
                        break;
                    case 5:
                        e.PropertyName.Should().Be(nameof(User.Is18));
                        r.Is18.Should().BeFalse();
                        break;
                    case 6:
                        e.PropertyName.Should().Be(nameof(User.IsUnder18));
                        r.IsUnder18.Should().BeTrue();
                        break;
                    case 7:
                        e.PropertyName.Should().Be(nameof(User.Age));
                        r.Age.Should().Be(18);
                        break;
                    case 8:
                        e.PropertyName.Should().Be(nameof(User.Is18));
                        r.Is18.Should().BeTrue();
                        break;
                    case 9:
                        e.PropertyName.Should().Be(nameof(User.IsUnder18));
                        r.IsUnder18.Should().BeFalse();
                        break;
                }
            };
            r.FirstName = "Pedro";
            r.LastName = "Gil";
            r.Age = 17;
            r.Age = 18;
            r.Name.Should().Be("Pedro Gil");
        }
    }

}