namespace Redux.Profunctor

open Encoding
open TypeClass.ProFunctor
open TypeClass.CoCartesian
open Kinds.Data.Either
open Redux

// Profunctor encoding for Reducer in the action dimension.
//
// ReducerActionPData<'st,'env,'a,'b> wraps a function 'st -> 'a -> 'env -> ('st × Effect<'b>)
// treated as a profunctor in the action coordinates ('a → 'b).
//
//   dimap f g  pre-maps action-in via f, post-maps Effect output via Effect.map g
//   right_     routes Right branch to the reducer + maps effects via Right;
//              Left branch → unchanged state + Effect.empty
//   left_      routes Left branch to the reducer + maps effects via Left;
//              Right branch → unchanged state + Effect.empty
//
// This is the CoCartesian profunctor encoding that lets `prismToP` zoom a
// Reducer's action coordinate: matching actions are dispatched, non-matching
// actions are silently ignored (the `outside` lens applied to a Reducer).
type ReducerActionPData<'st,'env,'a,'b> =
    | ReducerActionP of ('st -> 'a -> 'env -> 'st * Effect<'b>)
    interface H2<ReducerActionPEncoding<'st,'env>, 'a, 'b>

and ReducerActionPEncoding<'st,'env>() =
    inherit ProFunctor<ReducerActionPEncoding<'st,'env>>()

    // dimap f g: pre-map action-in via f, post-map effect output via Effect.map g.
    // State is returned as-is from the inner run.
    override __.dimap<'a,'b,'c,'d>
            (f: 'a -> 'b) (g: 'c -> 'd)
            (pbc: H2<ReducerActionPEncoding<'st,'env>,'b,'c>) =
        let (ReducerActionP run) = pbc :?> ReducerActionPData<'st,'env,'b,'c>
        ReducerActionP (fun s a r ->
            let (s', e) = run s (f a) r
            (s', Effect.map g e))
        :> H2<ReducerActionPEncoding<'st,'env>,'a,'d>

    interface CoCartesian<ReducerActionPEncoding<'st,'env>> with

        // right_ pab :: p (Either c a) (Either c b)
        // Right a  → run the reducer on a, map output effects via Right
        // Left  _  → return unchanged state + Effect.empty
        member __.right_<'a,'b,'c>
                (pab: H2<ReducerActionPEncoding<'st,'env>,'a,'b>) =
            let (ReducerActionP run) = pab :?> ReducerActionPData<'st,'env,'a,'b>
            ReducerActionP (fun s e r ->
                match e with
                | Right a ->
                    let (s', eff) = run s a r
                    (s', Effect.map Right eff)
                | Left _ ->
                    (s, Effect.empty))
            :> H2<ReducerActionPEncoding<'st,'env>, Either<'c,'a>, Either<'c,'b>>

        // left_ pab :: p (Either a c) (Either b c)
        // Left  a  → run the reducer on a, map output effects via Left
        // Right _  → return unchanged state + Effect.empty
        member __.left_<'a,'b,'c>
                (pab: H2<ReducerActionPEncoding<'st,'env>,'a,'b>) =
            let (ReducerActionP run) = pab :?> ReducerActionPData<'st,'env,'a,'b>
            ReducerActionP (fun s e r ->
                match e with
                | Left a ->
                    let (s', eff) = run s a r
                    (s', Effect.map Left eff)
                | Right _ ->
                    (s, Effect.empty))
            :> H2<ReducerActionPEncoding<'st,'env>, Either<'a,'c>, Either<'b,'c>>
