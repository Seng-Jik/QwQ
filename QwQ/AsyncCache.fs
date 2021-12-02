namespace QwQ

open System.Collections.Concurrent
open QwQ.Utils


type AsyncCache<'resourceId, 'resource> (factory: 'resourceId -> Async<'resource>) =
    
    let cache = ConcurrentDictionary<'resourceId, 'resource> ()

    member _.GetAsync (id: 'resourceId) =
        async {
            return cache.GetOrAdd (id, factory >> Async.RunSynchronously)
        }

    member x.GetAsyncProtected id = 
        x.GetAsync id |> Async.protect