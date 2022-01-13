module QwQ.Sources.Nozomi

open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru
open QwQ.Sources.SankakuComplex
open System
open FSharp.Data
open FSharp.Control


#nowarn "9"


let httpsOpt = 
    { HttpsOptions.Empty with 
        Headers = ("referer", "https://nozomi.la/") :: HttpsOptions.Empty.Headers }


let parseNozomiBin (nozomi: byte[]) =
    Seq.init 
        (Array.length nozomi / 4)
        (fun i -> 
            let span = nozomi.[i * 4 .. i * 4 + 3]

            let span =
                if BitConverter.IsLittleEndian
                then Array.rev span
                else span
            
            BitConverter.ToUInt32(span, 0))


type NozomiPostJson = JsonProvider<"https://j.nozomi.la/post/1/17/28749171.json">


let requestPost src (postId: uint32) =
    async {
        let jsonUrl = 
            let postId = string postId
            if String.length postId < 3
            then postId
            else $"https://j.nozomi.la/post/{postId.[^0]}/{postId.[^2 .. ^1]}/{postId}.json"

        let! json = NozomiPostJson.AsyncLoad jsonUrl
        
        return
            { Id = uint64 json.Postid
              Title = None
              Source = src
              Rating = Unrated
              SourceUrl = asyncSeq { yield $"https://nozomi.la/post/{json.Postid}.html" }
              Tags = 
                  [ json.Copyright
                    json.Character
                    json.Artist
                    json.General ]
                  |> Seq.concat
                  |> Seq.map (fun x -> x.TagnameDisplay.Replace(' ', '_'))
                  |> Seq.toList
              PreviewImage = 
                  json.Imageurl.Replace("//i.nozomi.la/", "//tn.nozomi.la/") + ".jpg" 
                  |> fixUrlPrefix
                  |> mapHttpsContent httpsOpt
                  |> Some
              Content = 
                  json.Imageurls
                  |> Array.map (fun x -> 
                      mapHttpsContent httpsOpt <| fixUrlPrefix x.Imageurl
                      |> AsyncSeq.singleton)
                  |> AsyncSeq.ofSeq }
    } 


let getTags (pageId: char) =
    async {
        let! json = JsonValue.AsyncLoad $"https://j.nozomi.la/search-{pageId}.json"

        return 
            json.Properties ()
            |> Seq.map fst
    }
    |> Async.protect


let newNozomiCache referer =
    AsyncCache<string, uint32 seq> (fun url -> 
        async {
            let! response = Http.AsyncRequestStream (url, headers = [ "referer", referer ])
            use byteStream = new IO.MemoryStream ()
            response.ResponseStream.CopyTo byteStream
            return 
                byteStream.ToArray ()
                |> parseNozomiBin
        })


type NozomiSource () =
    
    let nozomiCache = newNozomiCache "https://nozomi.la/"
    let getNozomi subDomain path =
        nozomiCache.GetAsync $"https://{subDomain}.nozomi.la/{path}.nozomi"

    interface ITags with
        member _.Tags =
            '0' :: ['a' .. 'z']
            |> List.map getTags
            |> AsyncSeq.ofSeqAsync
            |> AsyncSeq.map (function
                | Ok x -> Seq.map Ok x
                | Error x -> Seq.singleton <| Error x)
            |> AsyncSeq.concatSeq

    interface ISearchTag with
        member this.SearchTag s =
            let x = String.nullOrWhitespace s |> Option.map String.trim

            let getTags first = 
                asyncSeq {
                    let! s =
                        getTags first
                        |> Async.map (function
                            | Ok x -> Seq.map Ok x
                            | Error x -> Seq.singleton <| Error x)

                    yield! AsyncSeq.ofSeq s
                }

            match x with
            | None -> (this :> ITags).Tags
            | Some x when Seq.exists ((=) (System.Char.ToLower x.[0])) (seq { 'a' .. 'z' }) ->
                getTags (System.Char.ToLower x.[0])
            | Some _ -> getTags '0'
            |> AsyncSeq.filter (function
                | Ok x -> x.StartsWith s
                | _ -> true)
                

    interface ISource with
        member _.Name = "Nozomi"
        member this.AllPosts = 
            asyncSeq {
                let! nozomiBin = getNozomi 'n' "index"
                let posts = 
                    nozomiBin 
                    |> Seq.map (
                        requestPost this 
                        >> Async.map List.singleton 
                        >> Async.protect)
                
                yield! AsyncSeq.ofSeqAsync posts
            }

    interface IGetPostById with
        member this.GetPostById id =
            uint32 id
            |> requestPost this
            |> Async.protect
            |> Async.map (function
                | Ok x -> Ok <| Some x
                | Error e when e.Message.Contains "404" -> Ok <| None
                | Error x -> Error <| x)

    interface ISearch with
        member this.Search opt =
            let mapNozomi nozomi =
                match opt.Order with
                | Default | Date -> "nozomi/" + nozomi
                | Popular | Score -> $"nozomi/popular/{nozomi}-Popular"
                |> getNozomi 'j'
                |> Async.protect

            let firstTag, nextTags =
                match opt.Tags |> Seq.toList with
                | [] -> "index", [] : Tag * Tag list
                | a :: ls -> a, ls

            let firstTag, nextTags = 
                mapNozomi firstTag,
                AsyncSeq.ofSeq nextTags |> AsyncSeq.mapAsync mapNozomi
            
            asyncSeq {
                let! nextTags = Async.StartChild <| AsyncSeq.toListAsync nextTags

                match! firstTag with
                | Error x -> yield Error x
                | Ok src -> 
                    let! nextTags = nextTags
                    let nextTags, nextTagErrs =
                        nextTags |> List.choose (function Ok x -> Some x | _ -> None),
                        nextTags |> List.choose (function Error x -> Some <| Error x | _ -> None)

                    match nextTagErrs with
                    | _ :: _ -> yield! AsyncSeq.ofSeq nextTagErrs
                    | [] ->
                        yield! 
                            src
                            |> Seq.filter (fun postId ->
                                List.forall (Seq.exists ((=) postId)) nextTags)
                            |> AsyncSeq.ofSeq
                            |> AsyncSeq.mapAsync (requestPost this >> Async.map List.singleton >> Async.protect)
            }
            |> AntiGuro.antiThat opt.ExludeTags
                


let nozomi = NozomiSource () :> ISource