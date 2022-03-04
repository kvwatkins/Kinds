[<AutoOpen>]
module TypeClass.Category

#nowarn "1189"
#nowarn "1190"

open Encoding

// Category: morphisms that compose associatively with an identity.
//
//   id_      :: () -> a ~> a          (unit arg to avoid FS0671 generic property restriction)
//   compose  :: b ~> c -> a ~> b -> a ~> c
//
// Laws:
//   compose id_ f   = f               (left  identity)
//   compose f id_   = f               (right identity)
//   compose (compose f g) h = compose f (compose g h)  (associativity)
//
// Morphisms are encoded as H2<'cat,'a,'b>, consistent with the profunctor encoding.
//
// Canonical instances:
//   Fn  : id_ () = Fn id;  compose (Fn g) (Fn f) = Fn (g << f)
//   Kleisli (for a given Monad m)
type Category<'cat> =
    abstract member id_    <'a>         : unit              -> H2<'cat,'a,'a>
    abstract member compose<'a,'b,'c>   : H2<'cat,'b,'c>   -> H2<'cat,'a,'b> -> H2<'cat,'a,'c>

let catId<'a,'cat when 'cat :> Category<'cat>> () : H2<'cat,'a,'a> =
    (getTypeSignature<'cat> :> Category<'cat>).id_<'a> ()

let compose<'a,'b,'c,'cat when 'cat :> Category<'cat>>
    (g: H2<'cat,'b,'c>) (f: H2<'cat,'a,'b>) : H2<'cat,'a,'c> =
    (getTypeSignature<'cat> :> Category<'cat>).compose<'a,'b,'c> g f

module Category =
    /// Left-to-right composition  (f >>> g  in Haskell; avoids F# bit-shift operator)
    let andThen<'a,'b,'c,'cat when 'cat :> Category<'cat>>
        (f: H2<'cat,'a,'b>) (g: H2<'cat,'b,'c>) : H2<'cat,'a,'c> =
        compose<'a,'b,'c,'cat> g f
