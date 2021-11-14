namespace QwQ

open FSharp.Control


type IFavorite =
    inherit ISource
    abstract AddToFavorite: PostId -> Async<unit>
    abstract Favorites: AsyncSeq<PostPage>


type ISearchInFavorites =
    inherit ISource
    inherit IFavorite
    abstract SearchInFavorites: SearchPart -> AsyncSeq<PostPage>
