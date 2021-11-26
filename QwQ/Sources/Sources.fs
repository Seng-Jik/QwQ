module QwQ.Sources.Sources

open QwQ.Sources


let sources =
    [ yield! Moebooru.sources
      yield! Danbooru.sources
      yield! Gelbooru.sources
      yield! SankakuComplex.sources
      yield! TheBooruProject.sources
      yield! Shimmie.sources
      yield  NHentaiSharp.nhentai
      yield  Nozomi.nozomi ]

