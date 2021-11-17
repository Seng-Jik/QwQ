namespace QwQ.Console.Command

open QwQ.Console


type Command = 
    { Name: string
      ShortHelp: string
      LongHelp: string option
      Body: State ref -> string list -> unit }
    