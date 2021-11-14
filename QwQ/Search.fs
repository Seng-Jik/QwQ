namespace QwQ

open FSharp.Control


type Order =
    | Popular 
    | Date


type SearchPart =
    | Tag of Tag
    | NonTag of Tag
    | Rating of Rating
    | Order of Order


type ISearch =
    inherit ISource
    abstract Search: SearchPart -> AsyncSeq<PostPage>

