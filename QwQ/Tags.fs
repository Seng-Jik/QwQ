namespace QwQ

open FSharp.Control


type ITags =
    inherit ISource
    abstract Tags: AsyncSeq<Tag>


type ISearchTag =
    inherit ISource
    inherit ITags
    abstract SearchTag: string -> AsyncSeq<Tag>