module QwQ.Test.SearchPosts

open NUnit.Framework
open QwQ
open FSharp.Control


let search searchOpt source =
    match source: ISource with
    | :? ISearch as x -> Search.search x searchOpt
    | _ -> failwith "Do not support search."

    