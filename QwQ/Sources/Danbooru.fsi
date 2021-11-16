module QwQ.Sources.Danbooru

open QwQ
open FSharp.Control


val requestTags: 
    tagKey: string ->
    jsonUrlFromPage: (int -> string)
    -> AsyncSeq<Result<Tag, exn>>


val danbooru: ISource
val atfbooru: ISource
val sources: ISource list