# QwQ

QwQ是一个多图源壁纸下载工具。

[![Build & Test](https://github.com/Seng-Jik/QwQ/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/Seng-Jik/QwQ/actions/workflows/build.yml)
[![Full Test](https://github.com/Seng-Jik/QwQ/actions/workflows/full-test.yml/badge.svg)](https://github.com/Seng-Jik/QwQ/actions/workflows/full-test.yml)

## 支持的图源

图源      | 图源实现 | 特性
--------- | -------- | ---
Konachan  | [Moebooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Moebooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Yandere   | [Moebooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Moebooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Lolibooru | [Moebooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Moebooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
HypnoHub  | [Moebooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Moebooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Danbooru  | [Danbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Danbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
ATFBooru  | [Danbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Danbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Sonnohara | [Danbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Danbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Hijiribe  | [Danbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Danbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Safebooru Donmai  | [Danbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Danbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Gelbooru  | [Gelbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Gelbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
The Big ImageBoard (TBIB)  | [Gelbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Gelbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Safebooru | [Gelbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Gelbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
XBooru    | [Gelbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Gelbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Rule34    | [Gelbooru](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Gelbooru.fs) | IGetPostById, ISearch, ITags, ISearchTag
Sankaku Channel   | [Sankaku Complex](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/SankakuComplex.fs) | ISearch, ILogin<Username, Password>, ITags, ISearchTag, IGetPostById
Idol Complex      | [Sankaku Complex](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/SankakuComplex.fs) | ISearch, ILogin<Username, Password>, ISearchTag
All Girl          | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
Foot Fetish Booru | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
CGBooru           | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
Touhou            | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
Anime Girls 2020  | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
Character Library | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
Ecchi Booru       | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
Hina              | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
RuleXXX           | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch, IGetPostById
Nekobooru         | [Shimmie](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Shimmie.fs) | IGetPostById, ISearch, ITags, ISearchTag
Tentacle Rape     | [Shimmie](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Shimmie.fs) | IGetPostById, ISearch, ITags, ISearchTag
Fan Service       | [Shimmie](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Shimmie.fs) | IGetPostById, ISearch, ITags, ISearchTag
Rule34 Paheal     | [Shimmie](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Shimmie.fs) | IGetPostById, ISearch, ITags, ISearchTag
NHentai           | [NHentaiSharp](https://github.com/Xwilarg/NHentaiSharp) | IGetPostById, ISearch
Nozomi            | [Nozomi](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Nozomi.fs) | IGetPostById, ITags, ISearchTag, ISearch
Hitomi            | [Hitomi](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Hitomi.fs) | IGetPostById, ITags, ISearchTag, ISearch
Lolibaka          | [Lolibaka](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Lolibaka.fs) | IGetPostById, ISearch, ISearchTag
Booru.io          | [Booru.io](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/BooruIO.fs)  | ISearch, ISearchTag
The Hentai World  | [The Hentai World](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheHentaiWorld.fs) | ISearch, ISeatchTag, ITags


## 安装QwQ命令行

首先确保已经安装.NET 6 SDK。

```shell
dotnet pack ./QwQ.Console/QwQ.Console.fsproj -c Release
dotnet tool install -g QwQ.Console --add-source ./QwQ.Console/bin/Release
```

安装命令行工具后即可使用QwQ命令启动QwQ命令行。

命令行工具已经默认启用[AntiGuro](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/AniGuro.fs)。

## 关于Full Test

Full Test用于对整个图源中全部的内容和Tag进行测试以检查其兼容性。

### 执行Full Test

你可以在[Github Action](https://github.com/Seng-Jik/QwQ/actions/workflows/full-test.yml)上检查大部分内容，但仍有少部分图源因为内容过多导致无法在Github Action限制的6小时内完成测试。

你可以使用下方命令行手动执行Full Test。

```shell
dotnet fsi FullTest.fsx <图源> [--list-tags | --list-posts | --download-preview ]
```

### 后续支持计划

* iwara.tv / ecchi.iwara.tv
* Pixiv
* EHentai / ExHentai
* PicACG
* https://zha.erocool.me/
* https://www.simply-hentai.com/
* https://asmhentai.com/
* https://hentaihand.com/
* https://amanmi.com/
* https://nhentai.xxx/
* https://nhentai.com/

