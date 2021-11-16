namespace QwQ

open FSharp.Control


type IFavorite =
    abstract AddToFavorite: PostId -> Async<unit>
    abstract DeleteFromFavorite: PostId -> Async<unit>
    abstract ExistsInFavorite: PostId -> Async<bool>
    abstract Favorites: AsyncSeq<Result<PostPage, exn>>


type ISearchInFavorites =
    abstract SearchInFavorites: SearchOptions -> AsyncSeq<Result<PostPage, exn>>
