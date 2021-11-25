module QwQ.Sources.Danbooru

open QwQ
open FSharp.Control


val requestTags<'a,'b> :
    pageLoader: (int -> Async<Result<Result<'a, 'b> list, 'b>>) 
    -> AsyncSeq<Result<'a,'b>>


val danbooru: ISource
val atfbooru: ISource
val sonohara: ISource
val hijiribe: ISource 
val safebooruDonmai: ISource


val sources: ISource list