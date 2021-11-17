module QwQ.Console.Console


type Color = System.ConsoleColor


let private consoleLock = obj ()


let lockConsole f =
    lock consoleLock f


let kprintfc (c: Color) k f =
    let a = System.Console.ForegroundColor
    System.Console.ForegroundColor <- c
    Printf.kprintf (fun x -> 
        k x
        System.Console.ForegroundColor <- a) f


let printfc (c: Color) f = 
    kprintfc c (printf "%s") f


let printfnc (c: Color) f = 
    kprintfc c (printfn "%s") f

    