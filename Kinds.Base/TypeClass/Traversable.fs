[<AutoOpen>]
module TypeClass.Traversable

#nowarn "1189"
#nowarn "1190"

open Encoding

// Traversable: a Foldable Functor whose elements can be traversed
// in an applicative context.
//
//   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
//
// Laws:
//   traverse (pure_ << f)   = pure_ << fmap f                              (naturality)
//   traverse Identity       = Identity                                       (identity)
//   traverse (Compose << fmap g << f) = Compose << fmap (traverse g) << traverse f  (composition)
//
// sequenceA is derived: sequenceA = traverse id
//
// Encoded over H<'t,'a>.  The result nests H tags: H<'f, H<'t,'b>>.
type Traversable<'t when 't :> Functor<'t>> =
    abstract member traverse<'a,'b,'f when 'f :> Applicative<'f>>
        : ('a -> H<'f,'b>) -> H<'t,'a> -> H<'f, H<'t,'b> >

let traverse<'a,'b,'t,'f when 't :> Functor<'t> and 't :> Traversable<'t> and 'f :> Applicative<'f>>
    (f: 'a -> H<'f,'b>) (ta: H<'t,'a>) : H<'f, H<'t,'b> > =
    (getTypeSignature<'t> :> Traversable<'t>).traverse<'a,'b,'f> f ta

module Traversable =
    /// Evaluate each action in the structure and collect the results.
    /// sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
    let sequenceA<'a,'t,'f when 't :> Functor<'t> and 't :> Traversable<'t> and 'f :> Applicative<'f>>
        (tfa: H<'t, H<'f,'a> >) : H<'f, H<'t,'a> > =
        traverse<H<'f,'a>,'a,'t,'f> id tfa
