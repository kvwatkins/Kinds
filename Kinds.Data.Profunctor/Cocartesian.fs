[<AutoOpen>]
module TypeClass.CoCartesian

open Encoding
open Kinds.Data.Either

// Dual of Cartesian: threads sum types (Either) through a profunctor.
//
//   left_  :: p a b -> p (Either a c) (Either b c)
//   right_ :: p a b -> p (Either c a) (Either c b)
//
// Intuition: where Cartesian pairs extra data with the focus,
// CoCartesian routes one branch of a sum to the focus and passes the
// other branch through unchanged.
//
// Characterises Prism optics.
//
// Canonical instances:
//   (->) / Fn   : left_  f (Left  a) = Left  (f a); left_  f (Right c) = Right c
//   Tagged      : left_  (Tagged b) = Tagged (Left  b)
type CoCartesian<'p when 'p :> ProFunctor<'p>> =
    abstract member left_<'a,'b,'c>  : H2<'p,'a,'b> -> H2<'p, Either<'a,'c>, Either<'b,'c>>
    abstract member right_<'a,'b,'c> : H2<'p,'a,'b> -> H2<'p, Either<'c,'a>, Either<'c,'b>>

let left_<'a,'b,'c,'p when 'p :> ProFunctor<'p> and 'p :> CoCartesian<'p>>
    (x: H2<'p,'a,'b>) : H2<'p, Either<'a,'c>, Either<'b,'c>> =
    (getTypeSignature<'p> :> CoCartesian<'p>).left_<'a,'b,'c> x

let right_<'a,'b,'c,'p when 'p :> ProFunctor<'p> and 'p :> CoCartesian<'p>>
    (x: H2<'p,'a,'b>) : H2<'p, Either<'c,'a>, Either<'c,'b>> =
    (getTypeSignature<'p> :> CoCartesian<'p>).right_<'a,'b,'c> x
