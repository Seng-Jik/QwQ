namespace QwQ

open FSharp.Control


type TagType =
    | Copyright
    | Character
    | Artist
    | Circle
    | Style
    | General


type TagDetails =
    { Tag: Tag
      TagFromSource: ITags
      Count: uint64 
      Type: TagType }


and ITags =
    inherit ISource
    abstract Tags: AsyncSeq<Result<TagDetails, exn>>


type ISearchTag =
    inherit ISource
    inherit ITags
    abstract SearchTag: string -> AsyncSeq<Result<TagDetails, exn>>


module Tags =
    let allTags (x: ITags) = x.Tags
    let searchTag tagName (x: ISearchTag) = x.SearchTag tagName

    