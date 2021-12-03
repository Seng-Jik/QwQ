module QwQ.AntiGuro

open QwQ
open FSharp.Control


let private guroTags = 
    [
        // Lolibooru
        "guro"
        "asphyxiation"
        "autoerotic_asphyxiation"
        "archstanton"
        "crackaddict"
        "amputee"   // double_amputee quarter_amputee
        "death"
        "intestines"
        "wabaki"
        "eye_fuck"
        "eyes_rolled_back"
        "blood_on_face"
        "evil_grin"
        "evil_smile"
        "tongue_out"
        "decapitation"
        "head_fuck"
        "hair_grab"
        "blood_stain"
        "bloody_hair"
        "gua61"
        "dismemberment"
        "grin"
        "bruise"
        "cum_inflation"
        "empty_eyes"
        "necrophilia"
    ]


let private guroTagsProcessed =
    guroTags
    |> List.map (fun x -> x.Trim().ToLower())


let private isThatTag thoseTags (tag: string) =
    let tag = tag.Trim().ToLower()

    thoseTags
    |> Seq.exists (fun x -> tag.Contains ((x: string).Trim().ToLower()))


let hasThatTag thoseTags = Seq.exists <| isThatTag thoseTags


let isThatPost thoseTags (post: Post) =  hasThatTag thoseTags post.Tags


let antiThatPage thoseTags (postPage: PostPage) : PostPage =
    List.filter (isThatPost thoseTags >> not) postPage


let antiThat thoseTags (r: AsyncSeq<Result<PostPage, exn>>) =
    AsyncSeq.map (Result.map <| antiThatPage thoseTags) r


let antiGuro x = antiThat guroTagsProcessed x


