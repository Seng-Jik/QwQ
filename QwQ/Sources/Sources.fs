module QwQ.Sources.Sources

open QwQ.Sources


let sources =
    [ Moebooru.sources
      Danbooru.sources
      Gelbooru.sources
      SankakuComplex.sources
      TheBooruProject.sources
      Nekobooru.sources
      NHentaiSharp.sources ]
    |> List.concat

