[<AutoOpen>]
module TypeClass.Semigroup

#nowarn "1189"
#nowarn "1190"

open Encoding

// Semigroup: a type with an associative binary operation.
//
//   combine :: f a -> f a -> f a
//
// Laws:
//   combine (combine x y) z = combine x (combine y z)   (associativity)
//
// Encoded over H<'S,'a> so that the tag 'S carries the instance and
// the element type 'a is quantified, consistent with the rest of the library.
type Semigroup<'S> =
    abstract member combine<'a> : H<'S,'a> -> H<'S,'a> -> H<'S,'a>

type ``:: f a -> f a -> f a``<'S,'a> = H<'S,'a> -> H<'S,'a> -> H<'S,'a>

let combine<'a,'S when 'S :> Semigroup<'S>> : ``:: f a -> f a -> f a``<'S,'a> =
    (getTypeSignature<'S> :> Semigroup<'S>).combine<'a>
