module QwQ.Console.Dispatcher

open QwQ.Console.Console
open QwQ.Console.Command


let allCommands =
    [ Sources.commands ]
    |> List.concat


let help =
    {
        Name = "help"
        ShortHelp = "查看帮助"
        LongHelp = None
        Body = 
            fun _ ->
                function
                | [cmd] -> 
                    allCommands
                    |> List.tryFind (fun x -> x.Name = cmd)
                    |> function
                        | Some x -> 
                            printfnc Color.Green "%s - %s" x.Name x.ShortHelp
                            printfn ""
                            if x.LongHelp.IsSome
                            then printfn "%s" x.LongHelp.Value
                        | None -> printfnc Color.Red "Command %A not found." cmd
                | _ ->
                    printfnc Color.Green "Type 'help <cmd>' to get long help information for cmd."
                    printfn ""
                    allCommands
                    |> List.iter (fun x -> 
                        printfc Color.Magenta "  %s" <| x.Name.PadRight(20)
                        printfnc Color.Cyan "  %s" x.ShortHelp)
    }


let disptach state command args =
    (help :: allCommands)
    |> List.tryFind (fun x -> x.Name = command)
    |> function
        | None -> printfnc Color.Red "No command names %A" command
        | Some x -> 
            try x.Body state args 
            with e -> printfnc Color.Red "%A" e