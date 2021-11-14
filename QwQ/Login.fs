namespace QwQ


type LoginError =
    | WrongUserInfo


type ILogin<'a> =
    inherit ISource
    abstract Login: 'a -> Async<Result<unit, LoginError>>


type UsernamePassword = 
    { Username: string
      Password: string }


type ILoginUsernamePassword = ILogin<UsernamePassword>

