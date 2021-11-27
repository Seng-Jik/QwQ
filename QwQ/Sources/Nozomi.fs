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
    { HttpsOptions.Default with 
        Headers = ("referer", "https://nozomi.la/") :: HttpsOptions.Default.Headers }


let parseNozomiBin (nozomi: byte[]) =
    Seq.init 
        (Array.length nozomi / 4)
        (fun i -> 
            let span = ReadOnlySpan<byte>(nozomi, i * 4, 4)
            if BitConverter.IsLittleEndian
            then 
                let mem = NativeInterop.NativePtr.stackalloc<byte>(4)
                let stackSpan = Span<byte> (NativeInterop.NativePtr.toVoidPtr mem, 4)
                stackSpan.[0] <- span.[3]
                stackSpan.[1] <- span.[2]
                stackSpan.[2] <- span.[1]
                stackSpan.[3] <- span.[0]
                BitConverter.ToUInt32(stackSpan)
            else BitConverter.ToUInt32(span))


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
                      mapHttpsContent httpsOpt <| fixUrlPrefix x.Imageurl)
                  |> AsyncSeq.ofSeq
                  |> AsyncSeq.singleton }
    } 


type NozomiSource () =

    let mutable cachedNozomiBins = Map.empty

    let requestNozomiBin subDomain path =
        async {
            match Map.tryFind path cachedNozomiBins with
            | Some x -> return x
            | None ->
                let! response = Http.AsyncRequestStream ($"https://{subDomain}.nozomi.la/{path}.nozomi")
                use byteStream = new IO.MemoryStream ()
                response.ResponseStream.CopyTo byteStream
                return 
                    byteStream.ToArray ()
                    |> parseNozomiBin
        }

    interface ISource with
        member _.Name = "Nozomi"
        member this.AllPosts = 
            asyncSeq {
                let! nozomiBin = requestNozomiBin 'n' "index"
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
                |> requestNozomiBin 'j'
                |> Async.protect

            let firstTag, nextTags =
                match opt.Tags |> Seq.toList with
                | [] -> "index", [] : Tag * Tag list
                | a :: ls -> a, ls

            let firstTag, nextTags = 
                mapNozomi firstTag,
                AsyncSeq.ofSeq nextTags |> AsyncSeq.mapAsyncParallel mapNozomi
            
            let nozomiExcepts =
                AsyncSeq.ofSeq (Seq.map ((+) "nozomi/") opt.NonTags)
                |> AsyncSeq.mapAsyncParallel (requestNozomiBin 'j' >> Async.protect)
                |> AsyncSeq.choose (function Ok x -> Some x | _ -> None)
                |> AsyncSeq.concatSeq
            
            asyncSeq {
                let! excepts = 
                    Async.StartChild <|
                        async {
                            let enum = AsyncSeq.toBlockingSeq nozomiExcepts
                            return 
                                Collections.Generic.HashSet<uint32>(enum) 
                                :> Collections.Generic.IReadOnlySet<uint32>
                        }

                let! firstTag = Async.StartChild firstTag
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
                        let! excepts = excepts
                        yield! 
                            src
                            |> Seq.filter (fun postId ->
                                not (excepts.Contains postId)
                                && List.forall (Seq.exists ((=) postId)) nextTags)
                            |> AsyncSeq.ofSeq
                            |> AsyncSeq.mapAsyncParallel (requestPost this >> Async.map List.singleton >> Async.protect)
            }
                


let nozomi = NozomiSource () :> ISource