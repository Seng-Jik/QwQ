namespace QwQ

open FSharp.Control


type ITags =
    abstract Tags: AsyncSeq<Result<Tag, exn>>


type ISearchTag =
    abstract SearchTag: string -> AsyncSeq<Result<Tag, exn>>


module Tags =

    let allTags (x: ITags) = x.Tags
    let searchTag term (x: ISearchTag) = x.SearchTag term


    let slowSearchTag (term: string) (x: ITags) = 
        x.Tags
        |> AsyncSeq.filter (function
            | Ok x when not <| x.ToLower().Contains (term.ToLower()) -> false
            | _ -> true)

    