[<AutoOpen>]
module TypeClass.ProFunctor

#nowarn "1189"
#nowarn "1190"

open Encoding
open Pervasive
open Abbreviation

open Microsoft.FSharp.Core
//Type Classes
type ``:: (a -> b) -> (c -> d) -> p b c -> p a d``<'p,'a,'b,'c,'d> = ('a -> 'b) -> ('c -> 'd) ->H2<'p,'b,'c> -> H2<'p,'a,'d>
type ``:: (a -> b) -> p b c -> p a c``<'p,'a,'b,'c> = ('a -> 'b) ->H2<'p,'b,'c> -> H2<'p,'a,'c>
type ``:: (b -> c) -> p a b -> p a c``<'p,'a,'b,'c> = ('b -> 'c) ->H2<'p,'a,'b> -> H2<'p,'a,'c>

[<AbstractClass>]
type ProFunctor<'p>() =
        abstract member dimap<'a,'b,'c,'d> : ('a -> 'b) -> ('c -> 'd)   -> H2<'p,'b,'c> -> H2<'p,'a,'d>
        abstract member lmap <'a,'b,'c>    : ('a -> 'b) -> H2<'p,'b,'c> -> H2<'p,'a,'c>
        abstract member rmap <'a,'b,'c>    : ('b -> 'c) -> H2<'p,'a,'b> -> H2<'p,'a,'c>

        default this.lmap<'a,'b,'c> (a: 'a -> 'b) (b:  H2<'p,'b,'c>) = this.dimap a id b
        default this.rmap<'a,'b,'c> (a: 'b -> 'c) (b:  H2<'p,'a,'b>) = this.dimap id a b


// Cartesian (Strong) profunctor: lifts a profunctor through product types.
// first  :: p a b -> p (a, c) (b, c)   — thread extra type c through the first position
// second :: p a b -> p (c, a) (c, b)   — thread extra type c through the second position
type Cartesian<'p when 'p :> ProFunctor<'p>> =
        abstract member first <'a,'b,'c>  : H2<'p,'a,'b> -> H2<'p, 'a * 'c, 'b * 'c>
        abstract member second<'a,'b,'c>  : H2<'p,'a,'b> -> H2<'p, 'c * 'a, 'c * 'b>

let dimap<'a,'b,'c,'d, 'p when 'p :> ProFunctor<'p>> : ``:: (a -> b) -> (c -> d) -> p b c -> p a d``<'p,'a,'b,'c,'d> = getTypeSignature<'p>.dimap<'a,'b,'c,'d>
let lmap<'a,'b,'c,     'p when 'p :> ProFunctor<'p>> : ``:: (a -> b) -> p b c -> p a c``<'p,'a,'b,'c>                = getTypeSignature<'p>.lmap<'a,'b,'c>
let rmap<'a,'b,'c,     'p when 'p :> ProFunctor<'p>> : ``:: (b -> c) -> p a b -> p a c``<'p,'a,'b,'c>                = getTypeSignature<'p>.rmap<'a,'b,'c>

let first<'a,'b,'c,'p when 'p :> ProFunctor<'p> and 'p :> Cartesian<'p>>
    (x: H2<'p,'a,'b>) : H2<'p,'a*'c,'b*'c> =
    (getTypeSignature<'p> :> Cartesian<'p>).first<'a,'b,'c> x

let second<'a,'b,'c,'p when 'p :> ProFunctor<'p> and 'p :> Cartesian<'p>>
    (x: H2<'p,'a,'b>) : H2<'p,'c*'a,'c*'b> =
    (getTypeSignature<'p> :> Cartesian<'p>).second<'a,'b,'c> x
