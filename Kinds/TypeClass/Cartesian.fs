﻿module Kind.TypeClass.Cartesian

open Encoding
[<AbstractClass>]
type Cartesian<'p> =
        abstract member first<'a,'b,'c> : H2<'p,'b,'c> -> H2<'p,('a * 'c),('b * 'c)>
        abstract member second<'a,'b,'c> : H2<'p,'b,'c> -> H2<'p,('c * 'a),('c * 'b)>


//type ``:: (a -> b) -> f a -> f b``<'F,'a,'b> = ('a -> 'b) -> H<'F, 'a> -> H<'F, 'b>
//type ``:: a -> f b -> f b``<'F,'a,'b>        = 'a -> H<'F, 'b> -> H<'F, 'a>

//let fmap    <'a, 'b, 'F when 'F :> Functor<'F>> : ``:: (a -> b) -> f a -> f b`` <'F,'a,'b> = getTypeSignature<'F>.fmap
//let ``<$``  <'a, 'b, 'F when 'F :> Functor<'F>> : ``:: a -> f b -> f b``        <'F,'a,'b> = getTypeSignature<'F>.``<$``