[<AutoOpen>]
module TypeClass.CoStrong

open Encoding

// CoStrong profunctor: the categorical dual of Cartesian (Strong).
//
//   unfirst  :: p (a, d) (b, d) -> p a b   -- remove a threaded first component
//   unsecond :: p (d, a) (d, b) -> p a b   -- remove a threaded second component
//
// Where Cartesian *adds* extra context (products) to a profunctor,
// CoStrong *removes* it.
//
// The canonical Haskell implementation for the function arrow uses `fix`:
//
//   unfirst  f =  -> fst (fix ( -> f (a, snd b)))
//
// Because F# is strict, this diverges when forced. Implementations instead
// use `Unchecked.defaultof` as a seed value -- the strict approximation is
// correct for well-formed Colens optics where the extra component is
// threaded through unchanged.
//
// For CoStar<f> the implementation uses Comonad's `extend` and `extract`
// (see CoStar.fs).
//
// CoStrong characterises Colens optics (dual to Lens).
type CoStrong<'p when 'p :> ProFunctor<'p>> =
    abstract member unfirst<'a,'b,'c>  : H2<'p,'a*'c,'b*'c> -> H2<'p,'a,'b>
    abstract member unsecond<'a,'b,'c> : H2<'p,'c*'a,'c*'b> -> H2<'p,'a,'b>

let unfirst<'a,'b,'c,'p when 'p :> ProFunctor<'p> and 'p :> CoStrong<'p>>
    (x: H2<'p,'a*'c,'b*'c>) : H2<'p,'a,'b> =
    (getTypeSignature<'p> :> CoStrong<'p>).unfirst<'a,'b,'c> x

let unsecond<'a,'b,'c,'p when 'p :> ProFunctor<'p> and 'p :> CoStrong<'p>>
    (x: H2<'p,'c*'a,'c*'b>) : H2<'p,'a,'b> =
    (getTypeSignature<'p> :> CoStrong<'p>).unsecond<'a,'b,'c> x
