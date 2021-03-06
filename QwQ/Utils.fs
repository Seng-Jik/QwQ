module QwQ.Utils


module String =

    let nullOrWhitespace x =
        if System.String.IsNullOrWhiteSpace x
        then None
        else Some x


    let trim (x: string) = x.Trim()


module Result =

    let protect f =
        try Ok <| f ()
        with e -> Error e


    let toOption =
        function
        | Ok x -> Some x
        | Error _ -> None


    let unwrap = 
        function
        | Ok x -> x
        | Error x -> raise x


    let either rs =
        Seq.choose (function Ok x -> Some x | _ -> None) rs,
        Seq.choose (function Error x -> Some x | _ -> None) rs


    let eitherArray rs = either rs |> fun (a, b) -> Seq.toArray a, Seq.toArray b
    let eitherList rs = either rs |> fun (a, b) -> Seq.toList a, Seq.toList b


    let flattenAsync =
        function
        | Ok x -> x
        | Error x -> async { return Error x }

    
    type ResultBuilder () =
        member _.Bind(x, f) = Result.bind f x
        member _.Return(x) = Ok x
        member _.ReturnFrom(x) = x


let result = Result.ResultBuilder ()


module Option =

    let protect a = Result.protect a |> Result.toOption


    let ofResult = Result.toOption


    let unwrap =
        function
        | Some x -> x
        | None -> raise <| System.NullReferenceException ()


    type OptionBuilder () =
        member _.Bind(x, f) = Option.bind f x
        member _.Return(x) = Some x
        member _.ReturnFrom(x) = x


let option = Option.OptionBuilder ()


module Async =

    open System.Threading.Tasks


    let bind f a =
        async {
            let! a = a
            return! f a
        }


    let map f a =
        async {
            let! a = a
            return f a
        }


    let protect f = 
        async {
            try let! r = f in return Ok r
            with e -> return Error e
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


    let ofDelayedTask (f: unit -> Task<'a>) : Async<'a> =
        async { return! Async.AwaitTask <| f () }
