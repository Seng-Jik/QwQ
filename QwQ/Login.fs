namespace QwQ


type LoginError =
    | WrongUserInfo
    | LoginError of exn


type ILoggedIn<'Public> =
    inherit ISource
    abstract LoginInfo: Async<'Public>


type ILogin<'Public, 'Secret> =
    abstract Login: 'Public -> 'Secret -> Async<Result<ILoggedIn<'Public>, LoginError>>


module Login =
    
    let login public' secret (source: ILogin<'Public, 'Secret>) =
        source.Login public' secret

    
    let loginInfo (source: ILoggedIn<_>) =
        source.LoginInfo


type Username = string
type Password = string



