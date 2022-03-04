module Kinds.Data.Star
open Encoding
open TypeClass
open TypeClass.ProFunctor

// Star<'f,'d,'c> wraps the Kleisli arrow  'd -> H<'f,'c>
// It is a ProFunctor: dimap pre-processes the input and post-processes the output,
// leaving the effect 'f untouched.
//
//   dimap (a: 'a -> 'b) (b: 'c -> 'd) (Star s) = Star (fmap b << s << a)

type Star<'f,'d,'c when 'f :> Functor<'f>> = H2<StarEncoding<'f>,'d,'c>
and StarEncoding<'f when 'f :> Functor<'f>>() =
    inherit ProFunctor<StarEncoding<'f>>()

    override __.dimap<'a,'b,'c,'d> (a: 'a -> 'b) (b: 'c -> 'd) (c: H2<StarEncoding<'f>,'b,'c>) =
        let n = c :?> Star_Data<_,_,_>
        let z = match n with Star s -> s
        Star (fmap b << z << a) :> Star<'f,'a,'d>

    // Cartesian (Strong) instance: thread an extra type through the second element of a pair.
    //
    //   first  :: Star<F,a,b> -> Star<F,(a,c),(b,c)>
    //   second :: Star<F,a,b> -> Star<F,(c,a),(c,b)>
    interface Cartesian<StarEncoding<'f>> with
        member __.first<'a,'b,'c> (pab: H2<StarEncoding<'f>,'a,'b>) : H2<StarEncoding<'f>,'a*'c,'b*'c> =
            let (Star s): Star_Data<'f,'a,'b> = pab :?> _
            Star (fun (a, c) -> fmap (fun b -> (b, c)) (s a)) :> _
        member __.second<'a,'b,'c> (pab: H2<StarEncoding<'f>,'a,'b>) : H2<StarEncoding<'f>,'c*'a,'c*'b> =
            let (Star s): Star_Data<'f,'a,'b> = pab :?> _
            Star (fun (c, a) -> fmap (fun b -> (c, b)) (s a)) :> _

and Star_Data<'f, 'd,'c when 'f :> Functor<'f>> =
    | Star of ('d -> H<'f,'c>) interface Star<'f,'d,'c>

/// Extract the wrapped Kleisli arrow from a Star.
[<GeneralizableValue>]
let run<'f,'d,'c when 'f :> Functor<'f>>(m: Star<'f,'d,'c>): ('d -> H<'f,'c>) =
    let (Star f): Star_Data<'f,'d,'c> = m :?> _
    f

let (|Star|) (m: Star<'f,'d,'c>) = Star m
