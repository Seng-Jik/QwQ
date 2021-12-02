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
      Rating: Rating
      Order: Order }


type ISearch =
    inherit ISource
    abstract Search: SearchOptions -> AsyncSeq<Result<PostPage, exn>>


module Search =

    let search (x: ISearch) searchOpt = x.Search searchOpt


    let slowSearch (x: ISource) searchOpt =
        x.AllPosts
        |> AntiGuro.antiThat searchOpt.NonTags
        |> AsyncSeq.choose (function
            | Error x -> Some <| Error x
            | Ok page -> 
                match
                    page
                    |> List.filter (fun x -> 
                        (x.Rating = Unrated || x.Rating = searchOpt.Rating || searchOpt.Rating = Unrated)
                        && Seq.forall (fun tagNeed -> Seq.exists ((=) tagNeed) x.Tags) searchOpt.Tags)
                with
                | [] -> None
                | x -> Some <| Ok x)
