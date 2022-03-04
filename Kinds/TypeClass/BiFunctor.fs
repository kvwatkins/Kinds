[<AutoOpen>]
module TypeClass.BiFunctor

#nowarn "1189"
#nowarn "1190"

open Encoding

type BiFunctor<'P> =
    abstract member bimap<'a,'b,'c,'d> :('a -> 'b) -> ('c -> 'd) ->H2<'P,'a,'c> -> H2<'P,'b,'d>

type ``:: (a -> b) -> (c -> d) -> p a c -> p b d``<'P,'a,'b,'c,'d> = ('a -> 'b) -> ('c -> 'd) ->H2<'P, 'a,'c> -> H2<'P,'b,'d>

let bimap<'a,'b,'c,'d, 'P when 'P :> BiFunctor<'P>> : ``:: (a -> b) -> (c -> d) -> p a c -> p b d``<'P,'a,'b,'c,'d> = getTypeSignature<'P>.bimap<'a,'b,'c,'d>