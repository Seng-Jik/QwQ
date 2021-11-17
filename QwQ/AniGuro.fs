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
        "drill"
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
        "cut"   // cuts
        "grin"
        "bruise"
        "cum_inflation"
        "empty_eyes"
        "necrophilia"
    ]


let private guroTagsProcessed =
    guroTags
    |> List.map (fun x -> x.Trim().ToLower())


let private isGuroTag (tag: string) =
    let tag = tag.Trim().ToLower()

    guroTagsProcessed
    |> List.exists (fun x -> tag.Contains x)


let hasGuroTag = Seq.exists isGuroTag


let isGuroPost (post: Post) = hasGuroTag post.Tags


let antiGuroPage (postPage: PostPage) : PostPage =
    Seq.filter (isGuroPost >> not) postPage


let antiGuro (r: AsyncSeq<Result<PostPage, exn>>) =
    AsyncSeq.map (Result.map antiGuroPage) r

