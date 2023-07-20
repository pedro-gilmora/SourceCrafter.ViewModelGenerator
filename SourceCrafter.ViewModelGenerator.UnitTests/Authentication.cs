//using FacilCuba.Infrastructure.QvaPay;
//using FacilCuba.Views;
using HttPie.Generator;
using Microsoft.CodeAnalysis.CSharp.Syntax;
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
        internal Task EnsureAuthorization() => throw new NotImplementedException();

        //internal void OnTokenChange(object? sender, PropertyChangedEventArgs e)
        //{
        //    if (e.PropertyName == nameof(Token))
        //    {
        //        if (Token != null) { 
        //            _qvaPayClient.UpdateAuthenticationStatus(Token);
        //        }
        //    }
        //}

        private partial Task ExecuteLoginAsync() => throw new NotImplementedException();

        private partial Task ExecuteLogoutAsync() => throw new NotImplementedException();
    }
}