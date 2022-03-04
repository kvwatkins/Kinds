[<AutoOpen>]
module TypeClass.Applicative

#nowarn "1189"
#nowarn "1190"

open Encoding

// Applicative: a Functor with application.
//
//   pure_ :: a -> f a
//   ap    :: f (a -> b) -> f a -> f b
//
// Laws:
//   ap (pure_ id) v            = v                      (identity)
//   ap (ap (ap (pure_ (.)) u) v) w = ap u (ap v w)     (composition)
//   ap (pure_ f) (pure_ x)     = pure_ (f x)            (homomorphism)
//   ap u (pure_ y)             = ap (pure_ (fun f -> f y)) u  (interchange)
//
// fmap is derived: fmap f x = ap (pure_ f) x
[<AbstractClass>]
type Applicative<'F>() =
    inherit Functor<'F>()

    abstract member pure_ : 'a -> H<'F,'a>
    abstract member ap    : H<'F,'a -> 'b> -> H<'F,'a> -> H<'F,'b>

    // fmap derived from pure_ and ap; concrete instances may override for efficiency.
    override this.fmap<'a,'b> (f: 'a -> 'b) (x: H<'F,'a>) : H<'F,'b> =
        this.ap (this.pure_ f) x

type ``:: a -> f a``<'F,'a>             = 'a -> H<'F,'a>
type ``:: f (a -> b) -> f a -> f b``<'F,'a,'b> = H<'F,'a->'b> -> H<'F,'a> -> H<'F,'b>

let pure_<'a,'F when 'F :> Applicative<'F>> : ``:: a -> f a``<'F,'a> =
    getTypeSignature<'F>.pure_

let ap<'a,'b,'F when 'F :> Applicative<'F>> : ``:: f (a -> b) -> f a -> f b``<'F,'a,'b> =
    getTypeSignature<'F>.ap
