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
    |> Seq.map (fun postId -> 
        let postId = string postId
        if String.length postId < 3
        then postId
        else $"https://j.nozomi.la/post/{postId.[^0]}/{postId.[^2 .. ^1]}/{postId}.json")


type NozomiPostJson = JsonProvider<"https://j.nozomi.la/post/1/17/28749171.json">


let requestPost src (jsonUrl: string) =
    async {
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

    let requestNozomiBin path =
        async {
            match Map.tryFind path cachedNozomiBins with
            | Some x -> return x
            | None ->
                let! response = Http.AsyncRequestStream ($"https://n.nozomi.la/{path}.nozomi")
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
                let! nozomiBin = requestNozomiBin "index"
                let posts = 
                    nozomiBin 
                    |> Seq.map (
                        requestPost this 
                        >> Async.map List.singleton 
                        >> Async.protect)
                
                yield! AsyncSeq.ofSeqAsync posts
            }


let nozomi = NozomiSource () :> ISource