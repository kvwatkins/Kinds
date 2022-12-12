module Kind.Data.Star
open Encoding
open TypeClass

type Star<'f,'d,'c when 'f :> Functor<'f>> = H2<StarEncoding<'f>,'d,'c>
and StarEncoding<'f when 'f :> Functor<'f>>() =
    inherit ProFunctor<StarEncoding<'f>>() with
        override __.dimap<'a,'b,'c,'d> (a: 'a -> 'b) (b: 'c -> 'd) (c:H2<StarEncoding<'f>,'b,'c>) =
            let n = c :?> Star_Data<_,_,_>
            let z = match n with Star s -> s
            Star (fmap b << z << a ) :> Star<'f,'a,'d>
    end
and Star_Data<'f, 'd,'c when 'f :> Functor<'f>> =
    | Star of ('d -> H<'f,'c>) interface Star<'f,'d,'c>

[<GeneralizableValue>]
let run<'f,'d,'c when 'f :> Functor<'f>>(m: Star<'f,'d,'c>): ('d -> H<'f,'c> ) =
    let (Star f): Star_Data<'f,'d,'c> = m :?> _
    f

let (|Star|) (m: Star<'f,'d,'c>) = Star m