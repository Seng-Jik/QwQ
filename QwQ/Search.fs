namespace QwQ

open FSharp.Control


type Order =
    | Popular 


type SearchPart =
    | Tag of Tag
    | NonTag of Tag
    | Rating of Rating
    | Order of Order


type ISearchable =
    inherit ISource
    abstract Search: SearchPart -> AsyncSeq<PostPage>
