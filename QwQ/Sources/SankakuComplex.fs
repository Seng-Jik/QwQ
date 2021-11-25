module QwQ.Sources.SankakuComplex

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru
open QwQ.Sources.Danbooru


type PostListJson = JsonProvider<"./Sources/SankakuComplexSample.json">


let fixUrlPrefix (x: string) =
    if x.StartsWith "//" then "https:" + x
    else x


let mapPost httpsOption siteUrl src (json: PostListJson.Root) =
    { Id = uint64 json.Id
      Source = src
      Title = None
      Rating = mapRating json.Rating
      SourceUrl = 
          asyncSeq {
              $"{siteUrl}/post/show/{json.Id}"
              yield! 
                  json.Source
                  |> Option.bind String.nullOrWhitespace
                  |> Option.toList
                  |> AsyncSeq.ofSeq
          }

      Tags = 
          json.Tags 
          |> Seq.choose (fun x -> 
              x.Name
              |> Option.bind String.nullOrWhitespace
              |> Option.orElse x.NameEn
              |> Option.bind String.nullOrWhitespace
              |> Option.orElse x.NameJa
              |> Option.bind String.nullOrWhitespace)
          |> Seq.toList
            
      PreviewImage = 
          json.PreviewUrl 
          |> Option.bind String.nullOrWhitespace
          |> Option.map (fixUrlPrefix >> mapHttpsContent httpsOption)
          
      Content =
          asyncSeq {
              json.SampleUrl
              json.FileUrl
          }
          |> AsyncSeq.choose (Option.bind String.nullOrWhitespace)
          |> AsyncSeq.map (fixUrlPrefix >> mapHttpsContent httpsOption)
          |> AsyncSeq.singleton }


let loadJson parser httpsOptions uri =
    async {
        let! s = Http.AsyncRequestString(uri, headers = httpsOptions.Headers)
        return parser s
    }


type SankakuComplexSource (name, siteUrl, apiUrl, limit, loginStr, addtionalHttpHeaders) =

    member _.HttpsOptions =
        { Headers = HttpsOptions.Default.Headers @ addtionalHttpHeaders }

    member this.RequestPage urlPostfix pageId =
        requestPosts 
            (loadJson PostListJson.Parse this.HttpsOptions
                $"{apiUrl}?limit={limit}{loginStr}&page={pageId + 1}{urlPostfix}")
            (function
                | Error (:? System.Net.WebException as e) 
                    when e.Message.Contains "sign in to view more!"
                         || e.Message.Contains "You can only view up to"
                         || e.Message.Contains "snackbar__account_offset-forbidden" -> Ok []
                | x -> Result.map Array.toList x)
            (mapPost this.HttpsOptions siteUrl this)

    member this.RequestPostList urlPostfix =
        enumAllPages <| this.RequestPage urlPostfix
    
    interface ISource with
        member _.Name = name
        member x.AllPosts = x.RequestPostList ""

    interface ISearch with
        member x.Search search = 
            "&tags=" + mapSearchOptions { search with NonTags = [] }
            |> x.RequestPostList
            |> AntiGuro.antiThat search.NonTags


type SankakuChannelSource (addtionalHttpHeaders) =
    inherit SankakuComplexSource (
        "Sankaku Channel", 
        "https://chan.sankakucomplex.com",
        "https://capi-v2.sankakucomplex.com/posts",
        500,
        "",
        addtionalHttpHeaders)

    let requestTagList (this: SankakuChannelSource) urlPostfix =
        requestTags <| fun p ->
            requestPosts
                (loadJson JsonValue.Parse this.HttpsOptions
                    $"https://capi-v2.sankakucomplex.com/tags?limit=500&page={p + 1}{urlPostfix}")
                (Result.map (JsonExtensions.AsArray >> Array.toList))
                (fun json -> 
                    json.TryGetProperty "name"
                    |> Option.map JsonExtensions.AsString
                    |> Option.bind String.nullOrWhitespace
                    |> Option.orElse (json.TryGetProperty "name_en" |> Option.map JsonExtensions.AsString)
                    |> Option.bind String.nullOrWhitespace
                    |> Option.orElse (json.TryGetProperty "name_jp" |> Option.map JsonExtensions.AsString)
                    |> Ok)
        |> AsyncSeq.choose (function
            | Ok None -> None
            | Ok (Some x) -> Some <| Ok x
            | Error e -> Some <| Error e)

    interface IGetPostById with
        member x.GetPostById id =
            async {
                match!
                    (x :> ISearch).Search 
                        { Tags = [$"id:{id}"]
                          NonTags = []
                          Order = Default
                          Rating = set [ Safe; Questionable; Explicit ] }
                    |> AsyncSeq.tryFirst
                with
                | Some p -> return Result.map Seq.tryHead p
                | None -> return Ok None
            }

    interface ITags with
        member x.Tags = requestTagList x ""

    interface ISearchTag with
        member x.SearchTag s = requestTagList x $"&name={s}"


type SankakuChannelSourceLoggedIn (username, accessToken, refreshToken) =
    inherit SankakuChannelSource (["authorization", "Bearer " + accessToken])

    interface ILoggedIn<Username> with
        member _.LoginInfo = async { return username }


type SankakuChannelSourceGuest () =
    inherit SankakuChannelSource ([])

    interface ILogin<Username, Password> with
        member _.Login username password =
            async {
                let authUrl = "https://capi-v2.sankakucomplex.com/auth/token"
                let authJson = 
                    sprintf "{\"login\": \"%s\", \"password\": \"%s\"}"
                        username password
                
                let operation =
                    async {
                        let! response =
                            Http.AsyncRequestString(
                                authUrl, 
                                headers = 
                                    [ "User-Agent", HttpsOptions.DefaultUserAgent 
                                      "Content-Type", "application/json" ],
                                body = TextRequest authJson,
                                httpMethod = "POST")

                        let json = JsonValue.Parse response

                        return 
                            if json.["success"].AsBoolean ()
                            then 
                                Ok (json.["access_token"].AsString(),
                                    json.["refresh_token"].AsString())
                            else Error <| json.["error"].AsString()
                    }
                    |> Async.protect
                    
                match! operation with
                | Ok (Ok (acc, refr)) -> 
                    return Ok <| SankakuChannelSourceLoggedIn (username, acc, refr)
                | Ok (Error err) when err.Contains "invalid login or password" -> 
                    return Error WrongUserInfo
                | Error err when err.Message.Contains "invalid login or password" ->
                    return Error WrongUserInfo
                | Ok (Error err) -> return Error <| LoginError (exn (err))
                | Error err -> return Error <| LoginError err
            }


type IdolComplexSourceLoggedIn (username, loginStr) =
    inherit SankakuComplexSource (
        "Idol Complex", 
        "https://idol.sankakucomplex.com/",
        "https://iapi.sankakucomplex.com/post/index.json",
        60,
        loginStr,
        []) 

    interface ILoggedIn<Username> with
        member _.LoginInfo = async { return username }


type IdolComplexSource () =
    inherit SankakuComplexSource (
        "Idol Complex", 
        "https://idol.sankakucomplex.com/",
        "https://iapi.sankakucomplex.com/post/index.json",
        60,
        "",
        [])

    interface ILogin<Username, Password> with
        member _.Login username password =
            async {
                use sha1 = System.Security.Cryptography.SHA1.Create ()
                let pwHash =
                    sha1.ComputeHash(
                        $"choujin-steiner--{password}--"
                        |> System.Text.Encoding.UTF8.GetBytes)
                    |> Array.map (sprintf "%x")
                    |> Array.reduce (+)

                let loginStr = $"&login={username}&password_hash={pwHash}"

                let source = IdolComplexSourceLoggedIn (username, loginStr)

                match! source.RequestPage "" 26 with
                | Ok x when Seq.isEmpty x -> return Error WrongUserInfo
                | Ok _ -> return Ok (source :> ILoggedIn<Username>)
                | Error e -> return Error <| LoginError e
            }


let sankakuChannel = SankakuChannelSourceGuest () : ISource
let idolComplex = IdolComplexSource () :> ISource


let sources =
    [ sankakuChannel
      idolComplex ]

      