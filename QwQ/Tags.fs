namespace QwQ

open FSharp.Control


type ITags =
    inherit ISource
    abstract Tags: AsyncSeq<Result<Tag, exn>>


type ISearchTag =
    inherit ISource
    inherit ITags
    abstract SearchTag: string -> AsyncSeq<Result<Tag, exn>>


module Tags =
    let allTags (x: ITags) = x.Tags
    let searchTag tagName (x: ISearchTag) = x.SearchTag tagName

    