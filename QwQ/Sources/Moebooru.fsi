module QwQ.Sources.Moebooru

open QwQ
open FSharp.Control


val konachan: ISource
val yandere: ISource
val lolibooru: ISource
val hypnohub: ISource
val sources: ISource list


val mapRating: string -> Rating
val getFileNameFromUrl: string -> string
val mapHttpsContent: HttpsOptions -> string -> Content
val mapSearchRating: Set<Rating> -> string Option
val mapOrder: Order -> string option
val mapSearchOptions: SearchOptions -> string
val parseTags: string -> Tag list


val enumAllPages<'a,'b> : 
    getPageByIndex: (int -> Async<Result<'a list,'b>>) 
    -> AsyncSeq<Result<'a list,'b>>


val requestPosts<'a,'b,'c,'d> : 
    loadJson: Async<'a> -> 
    map: (Result<'a,exn> -> Result<'b list,'c>) -> 
    mapPost: ('b -> 'd) 
    -> Async<Result<'d list,'c>>