module Kinds.Data.Fn

open Encoding
open TypeClass
open TypeClass.ProFunctor

// The function arrow 'a -> 'b as a profunctor.
//
// This is the canonical "ground-truth" implementation of several profunctor
// type classes:
//
//   ProFunctor : dimap f g h = g . h . f            (precompose and postcompose)
//   Cartesian  : first  f (a, c) = (f a, c)         (thread second component)
//                second f (c, a) = (c, f a)         (thread first component)
//   Closed     : closed f g = f . g                 (postcompose into a function space)
//   Monoidal   : par f g (a, c) = (f a, g c)        (map components independently)
//                empty_ = id                        (identity on unit)
//   Category   : id_ () = Fn id;  compose g f = Fn (runFn g << runFn f)
//   Arrow      : arr f = Fn f
//
// CoStrong: implemented using the strict approximation of the Haskell `fix`-
// based definition.  The Haskell instance is:
//
//   unfirst  (Fn f) = Fn (\a -> fst (fix (\b -> f (a, snd b))))
//
// Because F# is strict, `fix` diverges here (forcing `b` requires `b`).
// Instead we seed with `Unchecked.defaultof<'d>` which is correct for any
// well-formed Colens optic where the extra component passes through unchanged.

type Fn<'a,'b> = H2<FnEncoding,'a,'b>
and FnEncoding() =
    inherit ProFunctor<FnEncoding>()

    override __.dimap<'a,'b,'c,'d> (f: 'a -> 'b) (g: 'c -> 'd) (pbc: H2<FnEncoding,'b,'c>) =
        let (Fn h) = pbc :?> Fn_Data<'b,'c>
        Fn (g << h << f) :> H2<FnEncoding,'a,'d>

    interface Cartesian<FnEncoding> with
        member __.first<'a,'b,'c> (pab: H2<FnEncoding,'a,'b>) =
            let (Fn f) = pab :?> Fn_Data<'a,'b>
            Fn (fun (a, c) -> (f a, c)) :> H2<FnEncoding,'a*'c,'b*'c>
        member __.second<'a,'b,'c> (pab: H2<FnEncoding,'a,'b>) =
            let (Fn f) = pab :?> Fn_Data<'a,'b>
            Fn (fun (c, a) -> (c, f a)) :> H2<FnEncoding,'c*'a,'c*'b>

    interface Closed<FnEncoding> with
        member __.closed<'x,'a,'b> (pab: H2<FnEncoding,'a,'b>) =
            let (Fn f) = pab :?> Fn_Data<'a,'b>
            // closed f = (f .) : ('x -> 'a) -> ('x -> 'b)
            Fn (fun (g: 'x -> 'a) -> f << g) :> H2<FnEncoding,'x->'a,'x->'b>

    interface Monoidal<FnEncoding> with
        member __.par<'a,'b,'c,'d> (p1: H2<FnEncoding,'a,'b>) (p2: H2<FnEncoding,'c,'d>) =
            let (Fn f) = p1 :?> Fn_Data<'a,'b>
            let (Fn g) = p2 :?> Fn_Data<'c,'d>
            Fn (fun (a, c) -> (f a, g c)) :> H2<FnEncoding,'a*'c,'b*'d>
        member __.empty_ =
            Fn id :> H2<FnEncoding,unit,unit>

    interface Category<FnEncoding> with
        member __.id_<'a> () =
            Fn id :> H2<FnEncoding,'a,'a>
        member __.compose<'a,'b,'c> (g: H2<FnEncoding,'b,'c>) (f: H2<FnEncoding,'a,'b>) =
            let (Fn g') = g :?> Fn_Data<'b,'c>
            let (Fn f') = f :?> Fn_Data<'a,'b>
            Fn (g' << f') :> H2<FnEncoding,'a,'c>

    interface Arrow<FnEncoding> with
        member __.arr<'b,'c> (f: 'b -> 'c) =
            Fn f :> H2<FnEncoding,'b,'c>

    interface CoStrong<FnEncoding> with
        member __.unfirst<'a,'b,'c> (pab: H2<FnEncoding,'a*'c,'b*'c>) =
            let (Fn f) = pab :?> Fn_Data<'a*'c,'b*'c>
            Fn (fun a ->
                let mutable d = Unchecked.defaultof<'c>
                let bd = f (a, d)
                d <- snd bd
                fst bd)
            :> H2<FnEncoding,'a,'b>
        member __.unsecond<'a,'b,'c> (pab: H2<FnEncoding,'c*'a,'c*'b>) =
            let (Fn f) = pab :?> Fn_Data<'c*'a,'c*'b>
            Fn (fun a ->
                let mutable d = Unchecked.defaultof<'c>
                let bd = f (d, a)
                d <- fst bd
                snd bd)
            :> H2<FnEncoding,'a,'b>

and Fn_Data<'a,'b> =
    | Fn of ('a -> 'b) interface Fn<'a,'b>

/// Extract the underlying function from a Fn value.
let runFn<'a,'b> (h: H2<FnEncoding,'a,'b>) : 'a -> 'b =
    let (Fn f) = h :?> Fn_Data<'a,'b> in f
