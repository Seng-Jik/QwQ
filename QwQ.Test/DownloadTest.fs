module QwQ.Test.DownloadTest

open NUnit.Framework
open QwQ
open QwQ.Utils
open FSharp.Control
open System.Net.Http
open System.IO


let ensureDir x =
    if Directory.Exists x |> not
    then Directory.CreateDirectory x |> ignore


let downloadToDir content dir =
    match content.DownloadMethod with
    | Https (url, { Headers = h }) ->
        use ws = new HttpClient ()
        for (k, v) in h do 
            ws.DefaultRequestHeaders.Add(k, v)

        let b = ws.GetByteArrayAsync(url).Result
        File.WriteAllBytes (dir + "/" + content.FileName, b)


let downloadTest (source: ISource) =
    let source = source :?> ISearch
    ensureDir "download-test"
    ensureDir $"download-test/{Source.name source}"

    async {
        match! 
            Search.search source { Tags = []; NonTags = []; Rating = [Safe]; Order = Popular }
            |> AsyncSeq.tryFirst
        with
        | None -> return failwith "No posts here!"
        | Some (Error e) -> return raise e
        | Some (Ok posts) ->
            return
                Seq.truncate 5 posts
                |> Seq.toArray
                |> Array.Parallel.map (fun post -> 
                    let baseDir = $"download-test/{Source.name source}/{post.Id}"
                    ensureDir baseDir
                    
                    post.PreviewImage 
                    |> Option.iter (fun p -> downloadToDir p baseDir)
                    
                    post.Content
                    |> AsyncSeq.iteriAsync (fun i x -> 
                        async {
                            let baseDir = baseDir + $"/Content {i}"
                            ensureDir baseDir
                            
                            do!
                                x 
                                |> AsyncSeq.iteriAsync (fun i x -> 
                                    async {
                                        let baseDir = baseDir + $"/Mipmap {i}"
                                        ensureDir baseDir

                                        downloadToDir x baseDir
                                    })
                        })
                    |> Async.RunSynchronously
                )
    }
    |> Async.Ignore
    |> Async.RunSynchronously


let [<Test>] ``download: Konachan`` () = downloadTest Sources.Moebooru.konachan
let [<Test>] ``download: Yandere`` () = downloadTest Sources.Moebooru.yandere
let [<Test>] ``download: Lolibooru`` () = downloadTest Sources.Moebooru.lolibooru
let [<Test>] ``download: HypnoHub`` () = downloadTest Sources.Moebooru.hypnohub

let [<Test>] ``download: Danbooru`` () = downloadTest Sources.Danbooru.danbooru
let [<Test>] ``download: ATFBooru`` () = downloadTest Sources.Danbooru.atfbooru
let [<Test>] ``download: Sonohara`` () = downloadTest Sources.Danbooru.sonohara
let [<Test>] ``download: Hijiribe`` () = downloadTest Sources.Danbooru.hijiribe

let [<Test>] ``download: Gelbooru`` () = downloadTest Sources.Gelbooru.gelbooru
let [<Test>] ``download: TBIB`` () = downloadTest Sources.Gelbooru.tbib
let [<Test>] ``download: Safebooru`` () = downloadTest Sources.Gelbooru.safebooru
let [<Test>] ``download: XBooru`` () = downloadTest Sources.Gelbooru.xbooru
let [<Test>] ``download: Rule34`` () = downloadTest Sources.Gelbooru.rule34

let [<Test>] ``download: Sankaku Channel`` () = downloadTest Sources.SankakuComplex.sankakuChannel
let [<Test>] ``download: Idol Complex`` () = downloadTest Sources.SankakuComplex.idolComplex
