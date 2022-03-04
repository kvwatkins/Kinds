[<AutoOpen>]
module TypeClass.Monoidal

open Encoding

// Monoidal profunctor: a profunctor equipped with a monoidal structure.
//
//   par    :: p a b -> p c d -> p (a, c) (b, d)   — combine two profunctors
//   empty_ :: p () ()                              — identity for par
//
// Note: this is the PROFUNCTOR monoidal structure — combining p a b with
// p c d to get p (a,c) (b,d). This is distinct from the value-level
// Monoid<'M> which combines H<'M,'a> with H<'M,'a>.
//
// Monoidal (also called "Traversing" in the van Laarhoven encoding)
// characterises Traversal optics — optics that can focus on zero or more
// elements simultaneously.
//
// Canonical instances:
//   (->) / Fn  : par f g = \(a,c) -> (f a, g c),  empty_ = id
//   Star f     : par + empty_ require Applicative f
type Monoidal<'p when 'p :> ProFunctor<'p>> =
    abstract member par<'a,'b,'c,'d> : H2<'p,'a,'b> -> H2<'p,'c,'d> -> H2<'p,'a*'c,'b*'d>
    abstract member empty_           : H2<'p,unit,unit>

let mpar<'a,'b,'c,'d,'p when 'p :> ProFunctor<'p> and 'p :> Monoidal<'p>>
    (x: H2<'p,'a,'b>) (y: H2<'p,'c,'d>) : H2<'p,'a*'c,'b*'d> =
    (getTypeSignature<'p> :> Monoidal<'p>).par<'a,'b,'c,'d> x y

let mempty_<'p when 'p :> ProFunctor<'p> and 'p :> Monoidal<'p>> () : H2<'p,unit,unit> =
    (getTypeSignature<'p> :> Monoidal<'p>).empty_
