namespace QwQ


type LoginError =
    | WrongUserInfo
    | Otherwise of exn


type ILogin<'a> =
    abstract Login: 'a -> Async<Result<unit, LoginError>>


type UsernamePassword = 
    { Username: string
      Password: string }


type ILoginUsernamePassword = ILogin<UsernamePassword>

