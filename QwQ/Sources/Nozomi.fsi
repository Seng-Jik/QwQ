module QwQ.Sources.Nozomi

open QwQ


val parseNozomiBin: byte[] -> uint32 seq
val newNozomiCache: referer: string -> AsyncCache<string, uint32 seq>

val nozomi: ISource
