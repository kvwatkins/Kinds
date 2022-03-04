namespace Redux

[<AutoOpen>]
module Reducer =
    open Effect

    type Reducer<'s, 'a, 'r>(x: 's -> 'a -> 'r -> 's * Effect<'a>) =
        member _.run = x

        member __.Compose(x: Reducer<'s, 'a, 'r>) =
            Reducer
                (fun s a r ->
                    let ls', le' = __.run s a r
                    let rs', re' = x.run ls' a r
                    (rs', le' + re'))

        static member empty =
            Reducer(fun (s: 's) (_: 'a) (_: 'r) -> (s, Effect.empty))

        static member (>.>)(lx: Reducer<'s, 'a, 'r>, rx) = lx.Compose(rx)

        static member merge(reducers: Reducer<'s, 'a, 'r> list) =
            reducers
            |> List.fold (>.>) Reducer<'s, _, _>.empty