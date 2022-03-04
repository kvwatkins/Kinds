namespace Redux

open Optics
open Reducer

[<Struct>]
type Fiber<'t, 's, 'b, 'a, 'u, 'r> =
    { state: Lens'<'t, 's>
      action: Prism'<'b, 'a>
      environment: 'u -> 'r }

[<AutoOpen>]
module Fiber =
    let pullback (x: Reducer<'s, 'a, 'r>) (fiber: Fiber<'t, 's, 'b, 'a, 'u, 'r>) =
        let x' =
            (fun (t: 't) (b: 'b) (u: 'u) ->
                match preview fiber.action b with
                | None -> (t, Effect.empty)
                | Some a ->
                    let s, lEff =
                        x.run (view fiber.state t) a (fiber.environment u)

                    let t' = (set fiber.state t s)
                    let gEff' = Effect.map (review fiber.action) lEff
                    (t', gEff'))

        Reducer(x')

    let scope (store: Store<'t, 'b, 'u>) (fiber: Fiber<'t, 's, 'b, 'a, 'u, 'r>) =
        let s' = (view fiber.state)
        let a' = (review fiber.action)

        { getState = fun _ -> s' (store.getState ())
          subscribe = fun notify -> store.subscribe (fun state -> notify (s' state))
          send = fun action -> store.send (a' action) }