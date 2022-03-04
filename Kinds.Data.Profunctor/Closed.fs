[<AutoOpen>]
module TypeClass.Closed

open Encoding

// Closed profunctor: lifts a profunctor through function types (exponentials).
//
//   closed :: p a b -> p (x -> a) (x -> b)
//
// Intuition: where Cartesian threads *product* types through a profunctor,
// Closed threads *function* (exponential) types. Given a way to transform
// 'a to 'b, Closed lets you transform ('x -> 'a) to ('x -> 'b) by
// composing on the right.
//
// Characterises Grate optics — structures defined by a "coexponential"
//   ((s -> a) -> b) -> t
// Any Closed profunctor can be lifted through a Grate.
//
// Canonical instances:
//   (->) / Fn   : closed f = (f .)       — precompose
//   CoStar f    : closed (CoStar k) = CoStar (fun fa x -> k (fmap ($ x) fa))
//   Tagged      : closed (Tagged b) = Tagged (fun _ -> b)
type Closed<'p when 'p :> ProFunctor<'p>> =
    abstract member closed<'x,'a,'b> : H2<'p,'a,'b> -> H2<'p,'x->'a,'x->'b>

let closed<'x,'a,'b,'p when 'p :> ProFunctor<'p> and 'p :> Closed<'p>>
    (x: H2<'p,'a,'b>) : H2<'p,'x->'a,'x->'b> =
    (getTypeSignature<'p> :> Closed<'p>).closed<'x,'a,'b> x
