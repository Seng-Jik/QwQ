module QwQ.Console.Main

open QwQ.Utils
open QwQ.Console.Console
open System


printfnc Color.Green "Welcome to QwQ Console!"
printfn ""
printfnc Color.Cyan "Type 'help' to get help."
printfn ""


let _ =
    Console.CancelKeyPress.Subscribe 
        (fun _ -> Console.ResetColor())


let rec parseCommandLine 
        (finished: char list list) 
        (curList: char list) 
        (until: char option) 
        (remain: char list) =
    match until, remain with
    | Some x, (c :: next)  when x = c -> 
        parseCommandLine (curList :: finished) [] None next
    | Some x, (c :: next) ->
        parseCommandLine finished (c :: curList) (Some x) next
    | Some _, [] -> parseCommandLine finished curList None []
    | None, (' ' :: next) -> parseCommandLine (curList :: finished) [] None next
    | None, ('\"' :: next) -> parseCommandLine finished curList (Some '\"') next
    | None, ('\'' :: next) -> parseCommandLine finished curList (Some '\'') next
    | None, (x :: next) -> parseCommandLine finished (x :: curList) None next
    | None, [] -> 
        (curList :: finished) 
        |> List.map (
            List.rev 
            >> Array.ofList 
            >> fun x -> String (x)) 
        |> List.choose String.nullOrWhitespace
        |> List.rev


let state = ref { Sources = [] }


while true do
    printfc Color.DarkBlue "QwQ! "
    Console.ReadLine() 
    |> Seq.toList 
    |> parseCommandLine [] [] None
    |> function
        | [] -> ()
        | command :: args ->
            Dispatcher.disptach state command args
            printfn ""