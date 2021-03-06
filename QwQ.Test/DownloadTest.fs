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
        try
            use ws = new HttpClient ()
            for (k, v) in h do 
                ws.DefaultRequestHeaders.Add(k, v)
        
            let b = ws.GetByteArrayAsync(url).Result
            File.WriteAllBytes (dir + "/" + content.FileName, b)
        with e ->
            lock stdout (fun () -> 
                printfn $"Warning: {dir}/{content.FileName} download failed:"
                printfn $"{e}"
            )

            Assert.Fail ()

    lock stdout (fun () -> printfn $"{dir}/{content.FileName}")


let downloadTest (source: ISource) =
    let downloadTimes = ref 0
    ensureDir "download-test"
    ensureDir $"download-test/{Source.name source}"

    async {
        match! 
            Source.allPosts source
            |> AntiGuro.antiGuro
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
                                        downloadTimes.Value <- downloadTimes.Value + 1
                                    })
                        })
                    |> Async.RunSynchronously
                )
    }
    |> Async.Ignore
    |> Async.RunSynchronously

    if downloadTimes.Value <= 0 then
        failwith "Download Nothing!!!"


let [<Test>] ``download: Konachan`` () = downloadTest Sources.Moebooru.konachan
let [<Test>] ``download: Yandere`` () = downloadTest Sources.Moebooru.yandere
let [<Test>] ``download: Lolibooru`` () = downloadTest Sources.Moebooru.lolibooru
let [<Test>] ``download: HypnoHub`` () = downloadTest Sources.Moebooru.hypnohub

let [<Test>] ``download: Danbooru`` () = downloadTest Sources.Danbooru.danbooru
let [<Test>] ``download: ATFBooru`` () = downloadTest Sources.Danbooru.atfbooru
let [<Test>] ``download: Sonohara`` () = downloadTest Sources.Danbooru.sonohara
let [<Test>] ``download: Hijiribe`` () = downloadTest Sources.Danbooru.hijiribe
let [<Test>] ``download: Safebooru Donmai`` () = downloadTest Sources.Danbooru.safebooruDonmai

let [<Test>] ``download: Gelbooru`` () = downloadTest Sources.Gelbooru.gelbooru
let [<Test>] ``download: TBIB`` () = downloadTest Sources.Gelbooru.tbib
let [<Test>] ``download: Safebooru`` () = downloadTest Sources.Gelbooru.safebooru
let [<Test>] ``download: XBooru`` () = downloadTest Sources.Gelbooru.xbooru
let [<Test>] ``download: Rule34`` () = downloadTest Sources.Gelbooru.rule34

let [<Test>] ``download: Sankaku Channel`` () = downloadTest Sources.SankakuComplex.sankakuChannel
let [<Test>] ``download: Idol Complex`` () = downloadTest Sources.SankakuComplex.idolComplex

let [<Test>] ``download: All Girl`` () = downloadTest Sources.TheBooruProject.allgirl
let [<Test>] ``download: Foot Fetish Booru`` () = downloadTest Sources.TheBooruProject.footfetishbooru
let [<Test>] ``download: CGBooru`` () = downloadTest Sources.TheBooruProject.cgbooru
let [<Test>] ``download: Touhou`` () = downloadTest Sources.TheBooruProject.touhou
let [<Test>] ``download: Anime Girls 2020`` () = downloadTest Sources.TheBooruProject.animegirls2020
let [<Test>] ``download: Character Library`` () = downloadTest Sources.TheBooruProject.characterlib
let [<Test>] ``download: Ecchi Booru`` () = downloadTest Sources.TheBooruProject.ecchibooru
let [<Test>] ``download: Hina`` () = downloadTest Sources.TheBooruProject.hina
let [<Test>] ``download: RuleXXX`` () = downloadTest Sources.TheBooruProject.rulexxx

let [<Test>] ``download: Nekobooru`` () = downloadTest Sources.Shimmie.nekobooru
let [<Test>] ``download: Tentacle Rape`` () = downloadTest Sources.Shimmie.tentacleRape
let [<Test>] ``download: Fan Service`` () = downloadTest Sources.Shimmie.fanservice
let [<Test>] ``download: Rule34 Paheal`` () = downloadTest Sources.Shimmie.rule34paheal

let [<Test>] ``download: NHentai`` () = downloadTest Sources.NHentaiSharp.nhentai

let [<Test>] ``download: Nozomi`` () = downloadTest Sources.Nozomi.nozomi
let [<Test>] ``download: Hitomi`` () = downloadTest Sources.Hitomi.hitomi

let [<Test>] ``download: Lolibaka`` () = downloadTest Sources.Lolibaka.lolibaka

let [<Test>] ``download: Booru io`` () = downloadTest Sources.BooruIO.booruio

let [<Test>] ``download: The Hentai World`` () = downloadTest Sources.TheHentaiWorld.thehentaiworld
