[<AutoOpen>]
module TypeClass.ProFunctor

#nowarn "1189"
#nowarn "1190"

open Encoding
open Microsoft.FSharp.Core
[<Interface>]
type ProFunctor<'P> =
        abstract member dimap<'a,'b,'c,'d> : ('a -> 'b) -> ('c -> 'd)   -> H2<'P,'b,'c> -> H2<'P,'a,'d>
        abstract member lmap <'a,'b,'c>    : ('a -> 'b) -> H2<'P,'b,'c> -> H2<'P,'a,'c>
        abstract member rmap <'a,'b,'c>    : ('b -> 'c) -> H2<'P,'a,'b> -> H2<'P,'a,'c>

type ``:: (a -> b) -> (c -> d) -> p b c -> p a d``<'P,'a,'b,'c,'d> = ('a -> 'b) -> ('c -> 'd) ->H2<'P,'b,'c> -> H2<'P,'a,'d>
type ``:: (a -> b) -> p b c -> p a c``<'P,'a,'b,'c> = ('a -> 'b) ->H2<'P,'b,'c> -> H2<'P,'a,'c>
type ``:: (b -> c) -> p a b -> p a c``<'P,'a,'b,'c> = ('b -> 'c) ->H2<'P,'a,'b> -> H2<'P,'a,'c>

let dimap<'a,'b,'c,'d, 'P when 'P :> ProFunctor<'P>> : ``:: (a -> b) -> (c -> d) -> p b c -> p a d``<'P,'a,'b,'c,'d> = getTypeSignature<'P>.dimap<'a,'b,'c,'d>
let lmap<'a,'b,'c,     'P when 'P :> ProFunctor<'P>> : ``:: (a -> b) -> p b c -> p a c``<'P,'a,'b,'c>                = getTypeSignature<'P>.lmap<'a,'b,'c>
let rmap<'a,'b,'c,     'P when 'P :> ProFunctor<'P>> : ``:: (b -> c) -> p a b -> p a c``<'P,'a,'b,'c>                = getTypeSignature<'P>.rmap<'a,'b,'c>