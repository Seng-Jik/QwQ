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
Idol Complex      | [Sankaku Complex](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/SankakuComplex.fs) | ISearch, ILogin<Username, Password>
All Girl          | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
Foot Fetish Booru | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
CGBooru           | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
Touhou            | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
Anime Girls 2020  | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
Character Library | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
Ecchi Booru       | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
Hina              | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
RuleXXX           | [The Booru Project](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/TheBooruProject.fs) | ISearch
Nekobooru         | [Shimmie](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Shimmie.fs) | IGetPostById, ISearch, ITags, ISearchTag
Tentacle Rape     | [Shimmie](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Shimmie.fs) | IGetPostById, ISearch, ITags, ISearchTag
Fan Service       | [Shimmie](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Shimmie.fs) | IGetPostById, ISearch, ITags, ISearchTag
NHentai           | [NHentaiSharp](https://github.com/Xwilarg/NHentaiSharp) | IGetPostById, ISearch
Nozomi            | [Nozomi](https://github.com/Seng-Jik/QwQ/blob/main/QwQ/Sources/Nozomi.fs) | IGetPostById, ISearch

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
dotnet publish ./QwQ/QwQ.fsproj -c Release -f netstandard2.0
dotnet fsi FullTest.fsx <图源> [--list-tags | --list-posts | --download-preview ]
```

### 后续支持计划

* EHentai / ExHentai
* Pixiv
* PicACG
* https://www.hbrowse.com/
* https://thehentaiworld.com/
* https://rule34.paheal.net/
* https://hentai-cosplays.com/
* https://kawaiihentai.com/category/animepics
* http://www.booru.realmofastra.ca/
* https://hitomi.la/
* https://www.tsumino.com/
* https://hentai2read.com/
* https://imhentai.xxx/
* https://manytoon.com/
* https://hentairead.com/
* https://www.simply-hentai.com/
* https://hentaifox.com/
* https://asmhentai.com/
* https://hentaihand.com/
* https://pururin.to/
* https://m-hentai.net/
* https://nhentai.xxx/ (using same system with NHentai.net?)
* https://nhentai.com/
* https://9hentai.to/
* https://doujins.com/
* iwara.tv / ecchi.iwara.tv

