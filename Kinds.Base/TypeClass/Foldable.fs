[<AutoOpen>]
module TypeClass.Foldable

#nowarn "1189"
#nowarn "1190"

open Encoding

// Foldable: a container type that can be reduced to a summary value.
//
//   foldMap :: Monoid m => (a -> m) -> t a -> m
//
// Laws:
//   foldMap id            = fold
//   foldMap (g << f)      = foldMap g << foldMap f    (fusion)
//
// All standard fold operations are derivable from foldMap.
// Encoded over H<'t,'a> so the tag 't carries the instance.
type Foldable<'t> =
    abstract member foldMap<'a,'m,'M when 'M :> Monoid<'M>> : ('a -> H<'M,'m>) -> H<'t,'a> -> H<'M,'m>

let foldMap<'a,'m,'t,'M when 't :> Foldable<'t> and 'M :> Monoid<'M>>
    (f: 'a -> H<'M,'m>) (ta: H<'t,'a>) : H<'M,'m> =
    (getTypeSignature<'t> :> Foldable<'t>).foldMap<'a,'m,'M> f ta

module Foldable =
    /// Fold a container of monoidal values using the Monoid instance.
    let fold<'a,'t,'M when 't :> Foldable<'t> and 'M :> Monoid<'M>>
        (ta: H<'t, H<'M,'a>>) : H<'M,'a> =
        foldMap<H<'M,'a>,'a,'t,'M> id ta
