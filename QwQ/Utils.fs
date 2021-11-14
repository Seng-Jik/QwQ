module QwQ.Utils


module Result =


    let protect f =
        try Ok <| f ()
        with e -> Error e


    let toOption =
        function
        | Ok x -> Some x
        | Error _ -> None

    
    type ResultBuilder () =
        member _.Bind(x, f) = Result.bind f x
        member _.Return(x) = Ok x
        member _.ReturnFrom(x) = x


let result = Result.ResultBuilder ()


module Option =


    let protect a = Result.protect a |> Result.toOption


    let ofResult = Result.toOption


    type OptionBuilder () =
        member _.Bind(x, f) = Option.bind f x
        member _.Return(x) = Some x
        member _.ReturnFrom(x) = x


let option = Option.OptionBuilder ()


module Async =


    let protect f = 
        async {
            match! Async.Catch f with
            | Choice1Of2 x -> return Ok x
            | Choice2Of2 x -> return Error x
        }


    let rec retryResult times interval f =
        async {
            match! f with
            | Ok x -> return Ok x
            | Error _ when times > 0 -> 
                do! Async.Sleep (interval: int)
                return! retryResult (times - 1) interval f
            | Error e -> return Error e
        }


    let rec retry times interval f =
        async {
            match! f with
            | Some x -> return Some x
            | None when times > 0 ->
                do! Async.Sleep (interval: int)
                return! retry (times - 1) interval f
            | None -> return None
        }

