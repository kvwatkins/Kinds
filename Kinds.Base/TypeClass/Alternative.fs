[<AutoOpen>]
module TypeClass.Alternative

#nowarn "1189"
#nowarn "1190"

open Encoding

// Alternative: an Applicative with choice and a neutral element.
//
//   empty_ :: () -> f a       (neutral element; unit arg avoids FS0671 generic property restriction)
//   altOr  :: f a -> f a -> f a
//
// Laws:
//   altOr empty_ x        = x                     (left  identity)
//   altOr x empty_        = x                     (right identity)
//   altOr (altOr x y) z   = altOr x (altOr y z)  (associativity)
//   ap (altOr f g) x      = altOr (ap f x) (ap g x)  (left distributivity — some instances)
//
// Defined as an *interface* (not abstract class) so that types already inheriting
// Monad<'M> (an abstract class chain) can also implement Alternative<'F>.
//
// Canonical instances:
//   Maybe : empty_ () = None;  altOr (Some a) _ = Some a;  altOr None b = b
//   List  : empty_ () = [];    altOr xs ys = xs @ ys
type Alternative<'F when 'F :> Applicative<'F>> =
    abstract member empty_<'a> : unit       -> H<'F,'a>
    abstract member altOr<'a>  : H<'F,'a>  -> H<'F,'a> -> H<'F,'a>

let altEmpty<'a,'F when 'F :> Applicative<'F> and 'F :> Alternative<'F>> () : H<'F,'a> =
    (getTypeSignature<'F> :> Alternative<'F>).empty_<'a> ()

let altOr<'a,'F when 'F :> Applicative<'F> and 'F :> Alternative<'F>>
    (x: H<'F,'a>) (y: H<'F,'a>) : H<'F,'a> =
    (getTypeSignature<'F> :> Alternative<'F>).altOr<'a> x y

module Alternative =
    /// Try each action in the list, returning the first success.
    let asum<'a,'F when 'F :> Applicative<'F> and 'F :> Alternative<'F>>
        (xs: H<'F,'a> list) : H<'F,'a> =
        List.foldBack (fun x acc -> altOr<'a,'F> x acc) xs (altEmpty<'a,'F> ())
