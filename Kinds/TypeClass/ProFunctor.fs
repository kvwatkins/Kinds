[<AutoOpen>]
module TypeClass.ProFunctor

#nowarn "1189"
#nowarn "1190"

open Encoding
open Pervasive
open Abbreviation

open Microsoft.FSharp.Core
//Type Clases
type ``:: (a -> b) -> (c -> d) -> p b c -> p a d``<'p,'a,'b,'c,'d> = ('a -> 'b) -> ('c -> 'd) ->H2<'p,'b,'c> -> H2<'p,'a,'d>
type ``:: (a -> b) -> p b c -> p a c``<'p,'a,'b,'c> = ('a -> 'b) ->H2<'p,'b,'c> -> H2<'p,'a,'c>
type ``:: (b -> c) -> p a b -> p a c``<'p,'a,'b,'c> = ('b -> 'c) ->H2<'p,'a,'b> -> H2<'p,'a,'c>

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


type BiFunctor<'p> =
    abstract member bimap<'a,'b,'c,'d> :('a -> 'b) -> ('c -> 'd) ->H2<'p,'a,'c> -> H2<'p,'b,'d>

type ``:: (a -> b) -> (c -> d) -> p a c -> p b d``<'p,'a,'b,'c,'d> = ('a -> 'b) -> ('c -> 'd) -> H2<'p, 'a,'c> -> H2<'p,'b,'d>

let bimap<'a,'b,'c,'d, 'p when 'p :> BiFunctor<'p>> : ``:: (a -> b) -> (c -> d) -> p a c -> p b d``<'p,'a,'b,'c,'d> = getTypeSignature<'p>.bimap<'a,'b,'c,'d>

[<AbstractClass>]
type ProFunctor<'p>() =
        abstract member dimap<'a,'b,'c,'d> : ('a -> 'b) -> ('c -> 'd)   -> H2<'p,'b,'c> -> H2<'p,'a,'d>
        abstract member lmap <'a,'b,'c>    : ('a -> 'b) -> H2<'p,'b,'c> -> H2<'p,'a,'c>
        abstract member rmap <'a,'b,'c>    : ('b -> 'c) -> H2<'p,'a,'b> -> H2<'p,'a,'c>

        default this.lmap<'a,'b,'c> (a: 'a -> 'b) (b:  H2<'p,'b,'c>) = this.dimap a id b
        default this.rmap<'a,'b,'c> (a: 'b -> 'c) (b:  H2<'p,'a,'b>) = this.dimap id a b


and Cartesian<'p when 'p :> ProFunctor<'p>> =
        abstract member first <'a,'b,'c>     :  H2<'p,'b,'c> -> H2<'p,('a * 'c),('b * 'c)>
        abstract member second <'a,'b,'c>    :  H2<'p,'b,'c> -> H2<'p,('c * 'a),('c * 'b)>

let dimap<'a,'b,'c,'d, 'p when 'p :> ProFunctor<'p>> : ``:: (a -> b) -> (c -> d) -> p b c -> p a d``<'p,'a,'b,'c,'d> = getTypeSignature<'p>.dimap<'a,'b,'c,'d>
let lmap<'a,'b,'c,     'p when 'p :> ProFunctor<'p>> : ``:: (a -> b) -> p b c -> p a c``<'p,'a,'b,'c>                = getTypeSignature<'p>.lmap<'a,'b,'c>
let rmap<'a,'b,'c,     'p when 'p :> ProFunctor<'p>> : ``:: (b -> c) -> p a b -> p a c``<'p,'a,'b,'c>                = getTypeSignature<'p>.rmap<'a,'b,'c>