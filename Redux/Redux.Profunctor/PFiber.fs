namespace Redux.Profunctor

open Encoding
open TypeClass.ProFunctor
open TypeClass.CoCartesian
open Kinds.Data.Either
open Redux

// PFiber<'t,'s,'b,'a,'u,'r> packages the three coordinate transformations needed
// to pull a local Reducer<'s,'a,'r> back to a global Reducer<'t,'b,'u>.
//
//   getState    : 't -> 's        — view the state coordinate
//   setState    : 't -> 's -> 't  — set the state coordinate
//   matchAction : 'b -> 'a option — preview the action coordinate
//   embedAction : 'a -> 'b        — embed into the action coordinate
//   environment : 'u -> 'r        — project the global environment to the local one
//
// Plain F# functions are used throughout — no dependency on any concrete optics library.
[<Struct>]
type PFiber<'t,'s,'b,'a,'u,'r> =
    { getState    : 't -> 's
      setState    : 't -> 's -> 't
      matchAction : 'b -> 'a option
      embedAction : 'a -> 'b
      environment : 'u -> 'r }

[<AutoOpen>]
module PFiber =

    // Compose the three coordinate zooms into a single profunctor pullback.
    //
    //   ppullback reducer fiber
    //     = reducer
    //       |> magnify fiber.environment   -- Reducer<'s,'a,'r> → Reducer<'s,'a,'u>
    //       |> zoom    (lens optic)        --                    → Reducer<'t,'a,'u>
    //       |> zoomAction (prism optic)    --                    → Reducer<'t,'b,'u>
    //
    // The H2 optic functions for zoom and zoomAction are built here from the raw
    // get/set/match/embed functions using first+dimap and right_+dimap on the
    // encoding instances defined in this assembly.
    // No concrete optics library dependency is required.
    let ppullback
            (r: Reducer<'s,'a,'r>)
            (fiber: PFiber<'t,'s,'b,'a,'u,'r>)
            : Reducer<'t,'b,'u> =
        let magnified = r |> magnify fiber.environment

        // Build a Cartesian profunctor optic from get/set using first + dimap.
        // first: H2<p,'s,'s> → H2<p,'s*'t,'s*'t>
        // dimap (t → (get t, t)) ((s',t) → set t s') : → H2<p,'t,'t>
        let lensOp
                (p: H2<ReducerStatePEncoding<'a,'u>,'s,'s>)
                : H2<ReducerStatePEncoding<'a,'u>,'t,'t> =
            let enc = ReducerStatePEncoding<'a,'u>()
            let q : H2<ReducerStatePEncoding<'a,'u>,'s * 't,'s * 't> =
                (enc :> Cartesian<ReducerStatePEncoding<'a,'u>>).first p
            enc.dimap
                (fun (t: 't) -> (fiber.getState t, t))
                (fun (s', t) -> fiber.setState t s')
                q

        let zoomed = magnified |> zoom lensOp

        // Build a CoCartesian profunctor optic from match/embed using right_ + dimap.
        // right_: H2<p,'a,'a> → H2<p,Either<'b,'a>,Either<'b,'a>>
        // dimap (b → match b → Right a | Left b) (Right a → embed a | Left b → b) : → H2<p,'b,'b>
        let prismOp
                (p: H2<ReducerActionPEncoding<'t,'u>,'a,'a>)
                : H2<ReducerActionPEncoding<'t,'u>,'b,'b> =
            let enc = ReducerActionPEncoding<'t,'u>()
            let q : H2<ReducerActionPEncoding<'t,'u>, Either<'b,'a>, Either<'b,'a>> =
                (enc :> CoCartesian<ReducerActionPEncoding<'t,'u>>).right_ p
            enc.dimap
                (fun (b: 'b) ->
                    match fiber.matchAction b with
                    | Some a -> Right a
                    | None   -> Left b)
                (function Right a -> fiber.embedAction a | Left b -> b)
                q

        zoomed |> zoomAction prismOp

    // Create a scoped Store<'s,'a,'r> from a global Store<'t,'b,'u> using a PFiber.
    // Uses the raw get/embed functions directly — no profunctor overhead needed for scoping.
    let pscope
            (store: Store<'t,'b,'u>)
            (fiber: PFiber<'t,'s,'b,'a,'u,'r>)
            : Store<'s,'a,'r> =
        { getState  = fun _     -> fiber.getState (store.getState ())
          subscribe = fun notify -> store.subscribe (fun t -> notify (fiber.getState t))
          send      = fun a     -> store.send (fiber.embedAction a) }
