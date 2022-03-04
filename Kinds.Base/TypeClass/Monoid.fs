[<AutoOpen>]
module TypeClass.Monoid

#nowarn "1189"
#nowarn "1190"

open Encoding

// Monoid: a Semigroup with an identity element.
//
//   empty   :: () -> f a   (unit arg required; F# disallows generic abstract properties)
//   combine :: f a -> f a -> f a
//
// Laws:
//   combine (empty ()) x = x                                        (left  identity)
//   combine x (empty ()) = x                                        (right identity)
//   combine (combine x y) z = combine x (combine y z)              (associativity, from Semigroup)
type Monoid<'M> =
    inherit Semigroup<'M>
    abstract member empty<'a> : unit -> H<'M,'a>

let empty<'a,'M when 'M :> Monoid<'M>> () : H<'M,'a> =
    (getTypeSignature<'M> :> Monoid<'M>).empty<'a> ()

let mconcat<'a,'M when 'M :> Monoid<'M>> (xs: H<'M,'a> list) : H<'M,'a> =
    let inst = getTypeSignature<'M>
    xs |> List.fold
            (fun acc x -> (inst :> Semigroup<'M>).combine<'a> acc x)
            ((inst :> Monoid<'M>).empty<'a> ())
