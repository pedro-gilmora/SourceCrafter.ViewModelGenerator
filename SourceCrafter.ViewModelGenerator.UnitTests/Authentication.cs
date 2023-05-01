//using FacilCuba.Infrastructure.QvaPay;
//using FacilCuba.Views;
using HttPie.Generator;
using System.ComponentModel;
using System.Text.Json;

namespace FacilCuba.ViewModels
{
    public partial class Authentication
    {
        //internal const string AUTH_KEY = "authToken";
        //internal readonly QvaPayClient _qvaPayClient;

        //public Authentication(QvaPayClient authenticationService)
        //{
        //    _qvaPayClient = authenticationService;
        //    PropertyChanged += OnTokenChange;
        //}
        internal async Task EnsureAuthorization()
        {
            //Token = await SecureStorage.Default.GetAsync(AUTH_KEY);
        }

        //internal void OnTokenChange(object? sender, PropertyChangedEventArgs e)
        //{
        //    if (e.PropertyName == nameof(Token))
        //    {
        //        if (Token != null) { 
        //            _qvaPayClient.UpdateAuthenticationStatus(Token);
        //        }
        //    }
        //}

        private partial async Task ExecuteLoginAsync()
        {
            //IsBusy = true;
            //var pwd = Password!;
            //Password = "";
            //Credentials content = new() { Email = Email!, Password = pwd };

            //try
            //{
            //    if (await _qvaPayClient.Auth.Login.PostAsync(content) is { Me: { } user, AccessToken: { } token, TokenType: { } type } response)
            //    {
            //        await SecureStorage.Default.SetAsync(AUTH_KEY, Token = $"{type} {token}");
            //        await Shell.Current.GoToAsync("//home");
            //        await Shell.Current.DisplayAlert("✅ Acceso exitoso", $"Bienvenido {user.Name} a QvaPay!", "Continuar");
            //    }
            //}
            //catch (HttpRequestException ex)
            //{
            //    var msg = (await ex.ReadExceptionContent<IDictionary<string, JsonElement>>())?.Values
            //        ?.SelectMany(Flatenize)
            //        ?.Join(a => $"\n🔸{a}");
            //    await Shell.Current.DisplayAlert("⛔ QvaPay: Error!", msg, "Ok");
            //}

            //IsBusy = false;
        }

        private static IEnumerable<string?> Flatenize(JsonElement o)
        {
            return o.ValueKind == JsonValueKind.Array
                ? o.EnumerateArray().Select(a => a.GetString())
                : new[] { o.GetString() };
        }

        private partial async Task ExecuteLogoutAsync()
        {
            //IsBusy = true;
            //var isOk = false;
            //try
            //{
            //    await _qvaPayClient.Auth.Logout.GetAsync(afterSend: async r => isOk = r is { IsSuccessStatusCode: true });
            //}
            //catch
            //{
            //}
            //if (isOk)
            //{
            //    IsBusy = false;
            //    _qvaPayClient.UpdateAuthenticationStatus(null);
            //    SecureStorage.Default.Remove(AUTH_KEY);
            //    await Shell.Current.GoToAsync("//home");
            //}
        }
    }
}