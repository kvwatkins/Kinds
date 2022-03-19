namespace Kind.Optics.Profunctor.Data

open Encoding
open Kinds.Optics
open Concrete
open TypeClass

module Optic =
    open TypeClass

    // forall p . (C0 p, ..., CN p) => p a b -> p s t
    type Optic<'p,'s,'t,'a,'b when 'p :> ProFunctor<'p>> = H4<OpticEncoding<'p>,'s,'t,'a,'b>
        and OpticEncoding<'p when 'p :> ProFunctor<'p>>() =
            interface ProFunctor<OpticEncoding<'p>> with
                member __.dimap<'a,'b,'c,'d> a b c = a b c
                member __.lmap <'a,'b,'c> a b = a b
                member __.rmap <'a,'b,'c> a b = a b

    type Optic<'p,'s,'t,'a,'b> =
        | Optic of (H2<'p,'a,'b> -> H2<'p,'s,'t>)
            with static member inline (Optic a) =  a

    type AdapterP<'p,'s,'t,'a,'b when 'p :> ProFunctor<'p>> =
        interface ProFunctor<'p> with

    type Optic'<'p,'s,'a> = Optic<'p,'s,'s,'a,'a>

    //Equality
    type AdapterP<'p> = H2<AdapterP<'p>,'s,'t,'a,'b> -> H2<'p,'s,'t>
    type AdapterP<'p,'s,'t,'a,'b when 'p :> ProFunctor<'p>>() =

        (*type Star<'f,'d,'c when 'f :> Functor<'f>> = H2<StarEncoding<'f>,'d,'c>
and StarEncoding<'f when 'f :> Functor<'f>>() =
    interface ProFunctor<StarEncoding<'f>> with*)

(*    inherit Functor<maybeBy<'F>>()
    override __.fmap<'a,'b> (a:'a->'b) (b:H<maybeBy<'F>,'a>) =
        match unwrap b  with
        | Some n -> Just (a n)
        | None -> Nothing*)

    //Iso
    type Iso<'P,'s,'t,'a,'b when 'P :> ProFunctor<'P>> = Optic<'P,'s,'t,'a,'b>
    type Iso'<'P,'s,'a when 'P :> ProFunctor<'P>> = Optic'<'P,'s,'a>
    //instance Profunctor (Adapter a b) where
    //dimap f g (Adapter o i) = Adapter (o · f) (g · i)

(*type Adapter'<'P,'s,'t,'a,'b when 'P :> ProFunctor<'P>> = H4<adapter<'P>,'s,'t,'a,'b>
and adapter<'P when 'P :> ProFunctor<'P>>() =
        interface ProFunctor<adapter<'P>> with
            member __.dimap<'a,'b,'c,'d> (a: 'a -> 'b) (b: 'c -> 'd) (c:H2<adapter<'P>,'b,'c>) =
                let n = c :?> AdapterData<_,_,_,_,_>
                let (za,zb) = match n with AdapterD (z1,z2) ->z1,z2
                AdapterD ((za a),(zb b))
and AdapterData<'P,'a,'b when 'P :> ProFunctor<'P>> =
    | AdapterD of H2<'P,'a,'b> interface Adapter'<'P,'a,'b,'a,'b>*)


//        member __.lmap<'a,'b,'c> (a: 'a -> 'b) (b:H2<StarEncoding<'f>,'b,'c>) = dimap a id b
 //       member __.rmap<'a,'b,'c> (a: 'b -> 'c) (b: H2<StarEncoding<'f>,'a,'b>) = dimap id a b
//    end
(*

and Adapter<'s, 't, 'a, 'b> =
    | Adapter Adapter(a,b)
    with interface adapter<Adapter<'s,'t,'a,'b>>


    [<Interface>]
    type Adapter<'P,'s,'t,'a,'b> =
            abstract member adapterC2P<'s,'t,'a,'b> : Adapter<'s,'t,'a,'b> -> H4<'P,'s,'t,'a,'b>
    and Adapter<'P,'s,'t,'a, 'b when 'P :> ProFunctor<'P>> = H4<ProFunctor<'P>,'s,'t,'a,'b>
    and AdapterEnc<'P when 'P :> ProFunctor<'P>>() =
        interface ProFunctor<AdapterEnc<'P>> with
            member __.dimap<'a,'b,'c,'d> (a: 'a -> 'b) (b: 'c -> 'd) (o: Optic) =
                let n = c :?> Concrete.Adapter<'a,'b,'c,'d>
                let z = match n with Adapter s -> s
                Adapter :> Adapter<'P,'a,'b,'c,'d>*)

//    let adapterC2P (a: Adapter<'s,'t,'a,'b>) =
//    type Adapter<'P, 's,'t,'a,'b>() =
//        Optic<AdapterP<'s,'t,'a,'b>,'s,'t,'a,'b>

//    type AdapterP<'P,'a,'b,'s,'t when 'P :> Optic<'P>> = H2<StarEncoding<'f>,'d,'c>
//        inherit Optic<'P>() with
//        abstract member adapterC2P<'s,'t,'a,'b> : ('s -> 'a) -> ('b -> 't) -> AdapterP
//
//    type AdapterP<'P,'a,'b,'s,'t> =
//        inherit Optic<'P,'a,'b,'s,'t>
//
//adapterC2P :: Adapter a b s t → AdapterP a b s t
//adapterC2P (Adapter o i) = dimap o i

//instance Profunctor (Adapter a b) where
//dimap f g (Adapter o i) = Adapter (o · f) (g · i)
//adapterC2P :: Adapter a b s t → AdapterP a b s t
//adapterC2P (Adapter o i) = dimap o i