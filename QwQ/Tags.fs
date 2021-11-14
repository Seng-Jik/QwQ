namespace QwQ

open FSharp.Control


type ITags =
    inherit ISource
    abstract Tags: AsyncSeq<Result<Tag, exn>>


type ISearchTag =
    inherit ISource
    inherit ITags
    abstract SearchTag: string -> AsyncSeq<Result<Tag, exn>>