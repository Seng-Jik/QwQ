module QwQ.Sources.Danbooru

open QwQ
open FSharp.Control


val requestTags<'a,'b,'c when 'a :> seq<Result<'b, 'c>>> : 
    pageLoader: (int -> Async<Result<'a, 'c>>) 
    -> AsyncSeq<Result<'b,'c>>


val danbooru: ISource
val atfbooru: ISource
val sources: ISource list