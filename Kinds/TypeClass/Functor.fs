[<AutoOpen>]
module TypeClass.Functor

#nowarn "1189"
#nowarn "1190"

open Encoding
open Pervasive
open Abbreviation

[<AbstractClass>]
type Functor<'F>() =
        abstract member   fmap<'a, 'b> : ('a -> 'b) -> H<'F, 'a> -> H<'F, 'b>
        abstract member ``<$``<'a, 'b> :  'a        -> H<'F, 'b> -> H<'F, 'a>

        default __.``<$``<'a,'b> (a:'a) (b:H<'F, 'b>) = compose __.fmap K a b

type ``:: (a -> b) -> f a -> f b``<'F,'a,'b> = ('a -> 'b) -> H<'F, 'a> -> H<'F, 'b>
type ``:: a -> f b -> f b``<'F,'a,'b>        = 'a -> H<'F, 'b> -> H<'F, 'a>

let fmap    <'a, 'b, 'F when 'F :> Functor<'F>> : ``:: (a -> b) -> f a -> f b`` <'F,'a,'b> = getTypeSignature<'F>.fmap
let ``<$``  <'a, 'b, 'F when 'F :> Functor<'F>> : ``:: a -> f b -> f b``        <'F,'a,'b> = getTypeSignature<'F>.``<$``

module Operators =
    let (<<|) a b = ``<$`` a b