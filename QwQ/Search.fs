namespace QwQ

open FSharp.Control


type Order =
    | Popular 
    | Default


type SearchOptions =
    { Tags: Tag seq
      NonTags: Tag seq 
      Rating: Rating seq
      Order: Order }


type ISearch =
    inherit ISource
    abstract Search: SearchOptions -> AsyncSeq<Result<PostPage, exn>>


module Search =
    let search (x: ISearch) searchOpt = x.Search searchOpt
    