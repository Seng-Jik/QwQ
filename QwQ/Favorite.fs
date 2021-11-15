namespace QwQ

open FSharp.Control


type IFavorite =
    inherit ISource
    abstract AddToFavorite: PostId -> Async<unit>
    abstract DeleteFromFavorite: PostId -> Async<unit>
    abstract ExistsInFavorite: PostId -> Async<bool>
    abstract Favorites: AsyncSeq<Result<PostPage, exn>>


type ISearchInFavorites =
    inherit ISource
    inherit IFavorite
    abstract SearchInFavorites: SearchOptions -> AsyncSeq<Result<PostPage, exn>>
