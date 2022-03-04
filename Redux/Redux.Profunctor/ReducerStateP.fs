namespace Redux.Profunctor

open Encoding
open TypeClass.ProFunctor
open Redux

// Profunctor encoding for Reducer in the state dimension.
//
// ReducerStatePData<'act,'env,'s,'t> wraps a function 's -> 'act -> 'env -> ('t × Effect<'act>)
// treated as a profunctor in the state coordinates ('s → 't).
//
//   dimap f g  pre-maps state-in via f, post-maps state-out via g
//   first      pairs extra 'c through the first position:  (s,c) → (t,c)
//   second     pairs extra 'c through the second position: (c,s) → (c,t)
//
// This is the Cartesian (Strong) profunctor encoding that lets `lensToP`
// zoom a Reducer's state coordinate without calling lens.get/lens.set directly.
type ReducerStatePData<'act,'env,'s,'t> =
    | ReducerStateP of ('s -> 'act -> 'env -> 't * Effect<'act>)
    interface H2<ReducerStatePEncoding<'act,'env>, 's, 't>

and ReducerStatePEncoding<'act,'env>() =
    inherit ProFunctor<ReducerStatePEncoding<'act,'env>>()

    // dimap f g: pre-compose the state-input with f, post-compose the state-output with g.
    // Action and environment are passed through unchanged.
    override __.dimap<'a,'b,'c,'d>
            (f: 'a -> 'b) (g: 'c -> 'd)
            (pbc: H2<ReducerStatePEncoding<'act,'env>,'b,'c>) =
        let (ReducerStateP run) = pbc :?> ReducerStatePData<'act,'env,'b,'c>
        ReducerStateP (fun s a r ->
            let (t, e) = run (f s) a r
            (g t, e))
        :> H2<ReducerStatePEncoding<'act,'env>,'a,'d>

    interface Cartesian<ReducerStatePEncoding<'act,'env>> with

        // first pab :: p (a,c) (b,c)
        // Runs the reducer on the 'a-part of the pair; threads 'c through unchanged.
        member __.first<'a,'b,'c>
                (pab: H2<ReducerStatePEncoding<'act,'env>,'a,'b>) =
            let (ReducerStateP run) = pab :?> ReducerStatePData<'act,'env,'a,'b>
            ReducerStateP (fun (s, c) a r ->
                let (t, e) = run s a r
                ((t, c), e))
            :> H2<ReducerStatePEncoding<'act,'env>,'a * 'c,'b * 'c>

        // second pab :: p (c,a) (c,b)
        // Runs the reducer on the 'a-part of the pair; threads 'c through unchanged.
        member __.second<'a,'b,'c>
                (pab: H2<ReducerStatePEncoding<'act,'env>,'a,'b>) =
            let (ReducerStateP run) = pab :?> ReducerStatePData<'act,'env,'a,'b>
            ReducerStateP (fun (c, s) a r ->
                let (t, e) = run s a r
                ((c, t), e))
            :> H2<ReducerStatePEncoding<'act,'env>,'c * 'a,'c * 'b>
