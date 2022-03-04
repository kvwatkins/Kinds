namespace Prelude

open System.Collections.Generic

[<AutoOpen>]
module Function =
    (*  -- Memoization function - http://www.fssnip.net/63/title/memoizeBy

        -- A cached version of Type.GetGenericArguments()
            let cachedGetGenericArguments : Type -> Type[] =
                memoizeBy (fun t -> t.FullName) (fun t -> t.GetGenericArguments()) *)
    let memoizeBy inputToKey f =
        let cache = Dictionary<_, _>()
        fun x ->
            let k = inputToKey x
            if cache.ContainsKey(k) then cache.[k]
            else let res = f x
                 cache.[k] <- res
                 res