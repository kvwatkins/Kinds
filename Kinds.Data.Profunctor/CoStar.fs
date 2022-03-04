module Kinds.Data.CoStar

open Encoding
open TypeClass
open TypeClass.ProFunctor

// CoStar is the dual of Star.
//
//   Star<'f,'d,'c>   wraps  'd        -> H<'f,'c>   (produce a wrapped output)
//   CoStar<'f,'d,'c> wraps  H<'f,'d>  -> 'c         (consume a wrapped input)
//
// As a ProFunctor, dimap pre-processes the input contravariantly (using fmap,
// since the input is already inside the functor 'f) and post-processes the bare
// output covariantly.
//
//   dimap (a: 'a -> 'b) (b: 'c -> 'd) (CoStar s : CoStar<'f,'b,'c>)
//     : CoStar<'f,'a,'d>
//   = CoStar (b << s << fmap a)
//             ^               ^
//             post-process    pre-process via fmap to lift 'a -> 'b into F 'a -> F 'b

type CoStar<'f,'d,'c when 'f :> Functor<'f>> = H2<CoStarEncoding<'f>,'d,'c>
and CoStarEncoding<'f when 'f :> Functor<'f>>() =
    inherit ProFunctor<CoStarEncoding<'f>>()

    override __.dimap<'a,'b,'c,'d> (a: 'a -> 'b) (b: 'c -> 'd) (c: H2<CoStarEncoding<'f>,'b,'c>) =
        let n = c :?> CoStar_Data<_,_,_>
        let z = match n with CoStar s -> s
        CoStar (b << z << fmap a) :> CoStar<'f,'a,'d>

    // CoStrong: uses `fmap` to pair each focused element with the seed `d`.
    //
    // The Haskell instance requires Comonad (uses `extend`/`extract`):
    //
    //   unfirst (Cokleisli k) = Cokleisli (\w ->
    //     fst (fix (\b -> k (extend (\w' -> (extract w', snd b)) w))))
    //
    // In F# (strict) `fix` diverges, so we use Unchecked.defaultof as a seed.
    // Plain `fmap` suffices here — for any well-formed Colens the `d` component
    // is threaded through unchanged, so the Comonad context is not needed.
    interface CoStrong<CoStarEncoding<'f>> with
        member __.unfirst<'a,'b,'c> (pab: H2<CoStarEncoding<'f>,'a*'c,'b*'c>) =
            let (CoStar k) = pab :?> CoStar_Data<'f,'a*'c,'b*'c>
            CoStar (fun (fa: H<'f,'a>) ->
                let mutable d = Unchecked.defaultof<'c>
                let bd = k (fmap (fun a -> (a, d)) fa)
                d <- snd bd
                fst bd)
            :> CoStar<'f,'a,'b>
        member __.unsecond<'a,'b,'c> (pab: H2<CoStarEncoding<'f>,'c*'a,'c*'b>) =
            let (CoStar k) = pab :?> CoStar_Data<'f,'c*'a,'c*'b>
            CoStar (fun (fa: H<'f,'a>) ->
                let mutable d = Unchecked.defaultof<'c>
                let bd = k (fmap (fun a -> (d, a)) fa)
                d <- fst bd
                snd bd)
            :> CoStar<'f,'a,'b>

    // Closed instance: lift CoStar through function types.
    //
    //   closed (CoStar k) :: CoStar f (x -> a) (x -> b)
    //   = CoStar (fun fa x -> k (fmap (fun f -> f x) fa))
    //
    // Given fa : F (x -> a) and x : x,
    // fmap (fun f -> f x) fa : F a   (apply each function in fa to x)
    // then k : F a -> b gives the result.
    interface Closed<CoStarEncoding<'f>> with
        member __.closed<'x,'a,'b> (pab: H2<CoStarEncoding<'f>,'a,'b>) =
            let (CoStar k) = pab :?> CoStar_Data<'f,'a,'b>
            CoStar (fun (fa: H<'f,'x->'a>) (x: 'x) ->
                k (fmap (fun (f: 'x -> 'a) -> f x) fa))
            :> CoStar<'f,'x->'a,'x->'b>

and CoStar_Data<'f,'d,'c when 'f :> Functor<'f>> =
    | CoStar of (H<'f,'d> -> 'c) interface CoStar<'f,'d,'c>

/// Extract the wrapped function from a CoStar.
[<GeneralizableValue>]
let run<'f,'d,'c when 'f :> Functor<'f>> (m: CoStar<'f,'d,'c>) : H<'f,'d> -> 'c =
    let (CoStar f): CoStar_Data<'f,'d,'c> = m :?> _
    f

let (|CoStar|) (m: CoStar<'f,'d,'c>) = CoStar m
