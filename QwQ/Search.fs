namespace QwQ

open FSharp.Control


type Order =
    | Popular 
    | Date
    | Score
    | Default


type SearchOptions =
    { Tags: Tag seq
      NonTags: Tag seq 
      Rating: Rating seq
      Order: Order }


type ISearch =
    abstract Search: SearchOptions -> AsyncSeq<Result<PostPage, exn>>


module Search =
    let search (x: ISearch) searchOpt = x.Search searchOpt
    