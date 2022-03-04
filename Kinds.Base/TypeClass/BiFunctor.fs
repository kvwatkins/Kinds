[<AutoOpen>]
module TypeClass.BiFunctor

#nowarn "1189"
#nowarn "1190"

open Encoding

type BiFunctor<'p> =
    abstract member bimap<'a,'b,'c,'d> :('a -> 'b) -> ('c -> 'd) ->H2<'p,'a,'c> -> H2<'p,'b,'d>

type ``:: (a -> b) -> (c -> d) -> p a c -> p b d``<'p,'a,'b,'c,'d> = ('a -> 'b) -> ('c -> 'd) -> H2<'p, 'a,'c> -> H2<'p,'b,'d>

let bimap<'a,'b,'c,'d, 'p when 'p :> BiFunctor<'p>> : ``:: (a -> b) -> (c -> d) -> p a c -> p b d``<'p,'a,'b,'c,'d> = getTypeSignature<'p>.bimap<'a,'b,'c,'d>
