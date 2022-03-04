namespace Redux
open Prelude

[<AutoOpen>]
module Effect =
    type Effect<'a>(run: ('a -> unit) -> unit) =
        let _run = run
        member e.run = _run
        static member map (f: 'a -> 'b) (e: Effect<'a>) : Effect<'b> = Effect(fun cb -> e.run (f >> cb))
        static member empty: Effect<'a> = Effect(fun _ -> ())
        static member pure' a : Effect<'a> = Effect(fun cb -> cb a)

        //Used to fire and forget effects
        //With the constraint of void they cannot ran directly e.g. calling Effect.run()
        static member runToVoid(effect: Effect<Never>) : Effect<'b> = Effect.map absurd effect

        //Used when combining effects together and the input of one is void
        static member runIgnoringInput(effect: Effect<'a>) : Effect<'b> =
            Effect(fun _ -> effect.run (fun _ -> ()))

        static member (+)(x: Effect<'a>, y: Effect<'a>) =
            let preservingSum callback =
                x.run callback
                y.run callback

            Effect(preservingSum)