module QwQ.Console.Command.Sources

open QwQ
open QwQ.Sources.Sources
open QwQ.Console.Console
open QwQ.Utils
open FSharp.Control


exception SourceNotFoundException of string


let getSrcByName name =
    sources
    |> List.tryFind (fun x -> x.Name = name)
    |> function
        | None -> raise <| SourceNotFoundException name
        | Some x -> x


let commands = [

    {
        Name = "ls-all-src"
        ShortHelp = "显示所有可用来源"
        LongHelp = None
        Body = fun _ _ ->
            sources 
            |> List.iter (fun x -> printfn "  %s" x.Name)
    }

    {
        Name = "cls"
        ShortHelp = "清屏"
        LongHelp = None
        Body = fun _ _ -> System.Console.Clear ()
    }

    {
        Name = "exit"
        ShortHelp = "退出"
        LongHelp = None
        Body = fun _ _ -> exit 0
    }

    {
        Name = "ls-src"
        ShortHelp = "查看已经激活的来源"
        LongHelp = None
        Body = fun s _ ->
            s.Value.Sources
            |> List.iter (fun x -> printfn "  %s" x.Name)
    }

    {
        Name = "add-src"
        ShortHelp ="添加来源到激活来源列表"
        LongHelp = Some "add-src <source...>"
        Body = fun state src ->
            src
            |> List.iter (fun srcName ->
                if state.Value.Sources |> List.exists (fun x -> x.Name = srcName)
                then printfnc Color.Red "%s 已经存在于列表中。" srcName
                else 
                    state.Value <- 
                        { state.Value with 
                            Sources = getSrcByName srcName :: state.Value.Sources }
                    printfnc Color.Green "%s 已经添加成功。" srcName )
    }
    
    {
        Name = "del-src"
        ShortHelp = "从激活来源列表中删除来源"
        LongHelp = Some "del-src <source...>"
        Body = fun state srcToDel ->
            state.Value <-
                { state.Value with
                    Sources = 
                        List.filter 
                            (fun x -> not <| List.exists ((=) x.Name) srcToDel) 
                            state.Value.Sources }
    }

    {
        Name = "login-src-usrpwd"
        ShortHelp = "登录一个来源并加入到激活列表"
        LongHelp = Some "login-src-username-password <source> <username> <password>"
        Body = fun state ->
            function
            | src :: usr :: pw :: [] ->
                (getSrcByName src :?> ILogin<Username, Password>)
                |> Login.login usr pw
                |> Async.RunSynchronously
                |> function
                    | Ok source ->
                        state.Value <- { state.Value with Sources = source :: state.Value.Sources }
                    | Error WrongUserInfo -> failwith "用户名或密码可能不正确。"
                    | Error (LoginError e) -> raise e
            | _ -> invalidArg "" ""
    }

    {
        Name = "download"
        ShortHelp = "搜索并下载到当前目录"
        LongHelp = Some "download <tags...>"
        Body = fun state tags ->
            let search = 
                { Tags = tags
                  NonTags = []
                  Rating = set [Safe; Questionable; Explicit]
                  Order = Default }

            let dir = 
                tags 
                |> List.fold (fun a b -> a + " " + b) "" 
                |> String.trim
                |> String.nullOrWhitespace
                |> Option.defaultValue "no_tags"

            if System.IO.Directory.Exists dir |> not
            then System.IO.Directory.CreateDirectory dir |> ignore

            state.Value.Sources
            |> List.choose (function
                | :? ISearch as s -> Some (s.Name, AntiGuro.antiGuro (Search.search s search))
                | _ -> None)
            |> List.toArray
            |> Array.Parallel.iter (fun (name, result) ->
                AsyncSeq.iteriAsync (fun i x -> async {
                    match x with
                    | Error e ->
                        lockConsole (fun () ->
                            printfnc Color.Red "%s: Can not get page %d." name i
                            printfnc Color.Red "%A" e
                        )
                        return ()
                    | Ok page ->
                        for post in page do
                            let downloadContents dir =
                                AsyncSeq.iteriAsync (fun index mipmaps -> async {
                                    match! AsyncSeq.tryLast mipmaps with
                                    | Some content -> 
                                        match content.DownloadMethod with
                                        | Https (url, opt) ->
                                            let! result =
                                                Async.protect (async {
                                                    use w = new System.Net.Http.HttpClient ()
                                                    for (k, v) in opt.Headers do
                                                        w.DefaultRequestHeaders.Add(k, v)
                                                        
                                                    let b = w.GetByteArrayAsync(url).Result
                                                    System.IO.File.WriteAllBytes(dir + "/" + content.FileName, b) })
                                                |> Async.retryResult 5 5000

                                            match result with
                                            | Error e -> 
                                                lockConsole (fun () ->
                                                    printfnc Color.Red $"{name}: {post.Id} ({index}) download error:"
                                                    printfnc Color.Red "%A" e)
                                            | Ok () ->
                                                lockConsole (fun () -> 
                                                    printfnc Color.Green $"{name}: {post.Id} ({index}) downloaded.")
                                            
                                    | _ -> return ()
                                })

                            let contents = AsyncSeq.cache post.Content

                            let dir =
                                if (AsyncSeq.length contents |> Async.RunSynchronously) = 1L
                                then dir
                                else dir + "/" + (Option.defaultValue ("") post.Title)

                            if System.IO.Directory.Exists dir |> not
                            then System.IO.Directory.CreateDirectory dir |> ignore

                            contents
                            |> downloadContents dir
                            |> Async.RunSynchronously
                        return ()

                }) result
                |> Async.RunSynchronously
            )
    }
]