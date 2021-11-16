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
val mapSearchRating: Rating seq -> string Option
val mapOrder: Order -> string option
val mapSearchOptions: SearchOptions -> string
val parseTags: string -> AsyncSeq<Tag>


val enumAllPages<'a,'b,'c when 'a :> seq<'b>> : 
    getPageByIndex: (int -> Async<Result<'a,'c>>) 
    -> AsyncSeq<Result<'a,'c>>


val requestPosts<'a,'b,'c,'d,'e when 'b :> seq<'c>> : 
    loadJson: Async<'a> -> 
    map: (Result<'a,exn> -> Result<'b, 'd>) -> 
    mapPost: ('c -> 'e) 
    -> Async<Result<seq<'e>,'d>>