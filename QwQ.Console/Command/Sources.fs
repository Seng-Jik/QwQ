module QwQ.Console.Command.Sources

open QwQ
open QwQ.Sources.Sources
open QwQ.Console.Console


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
]