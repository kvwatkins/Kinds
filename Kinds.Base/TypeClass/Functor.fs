[<AutoOpen>]
module TypeClass.Functor

#nowarn "1189"
#nowarn "1190"

open Encoding
open Pervasive
open Abbreviation

[<AbstractClass>]
type Functor<'f>() =
        abstract member   fmap<'a, 'b> : ('a -> 'b) -> H<'f, 'a> -> H<'f, 'b>
        abstract member ``<$``<'a, 'b> :  'a        -> H<'f, 'b> -> H<'f, 'a>

        default __.``<$``<'a,'b> (a:'a) (b:H<'f, 'b>) = compose __.fmap K a b

type ``:: (a -> b) -> f a -> f b``<'f,'a,'b> = ('a -> 'b) -> H<'f, 'a> -> H<'f, 'b>
type ``:: a -> f b -> f b``<'f,'a,'b>        = 'a -> H<'f, 'b> -> H<'f, 'a>

let fmap    <'a, 'b, 'f when 'f :> Functor<'f>> : ``:: (a -> b) -> f a -> f b`` <'f,'a,'b> = getTypeSignature<'f>.fmap
let ``<$``  <'a, 'b, 'f when 'f :> Functor<'f>> : ``:: a -> f b -> f b``        <'f,'a,'b> = getTypeSignature<'f>.``<$``

module Operators =
    let (<<|) a b = ``<$`` a b
