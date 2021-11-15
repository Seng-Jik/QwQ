module QwQ.Sources.Moebooru

open QwQ
open FSharp.Data
open FSharp.Control


val konachan: ISource
val yandere: ISource
val lolibooru: ISource
val hypnohub: ISource
val sources: ISource list


val mapTag: JsonValue -> string
val mapRating: string -> Rating
val getFileNameFromUrl: string -> string
val mapHttpsContent: HttpsOptions -> string -> Content
val enumAllPages<'a,'b,'c when 'a :> seq<'b>> : (int -> Async<Result<'a,'c>>) -> AsyncSeq<Result<'a,'c>>
val mapSearchRating: Rating seq -> string Option
