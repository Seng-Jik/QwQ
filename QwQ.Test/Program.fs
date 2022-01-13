open NUnit.Framework

module Program =
    [<assembly: Parallelizable(ParallelScope.All)>]
    do 
        ()

    [<EntryPoint>]
    let main _ = 0
