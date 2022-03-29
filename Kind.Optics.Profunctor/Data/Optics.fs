namespace Kind.Optics.Profunctor.Data

open Encoding
open Kinds.Optics
open Concrete
open TypeClass

module Optics =
   (* Problem with concrete optics *)
   type R1 = {name: string; age:int}
   type R2 = {name2: string; age2:int}
   type Person = {name3: char; age3:float}

   [<Struct>] type Const<'t,'u> = Const of 't
   let getConst (Const t ) = t

   type ConcreteIso<'s,'t,'a,'b> = ConcreteIso of ('s -> 'a) * ('b -> 't) with
       member this.to_  = match this with ConcreteIso(to_,_) -> to_
       member this.from = match this with ConcreteIso(_,from) -> from
       member l.Compose (r: ConcreteIso<'a,'b,'c,'d>) :  ConcreteIso<'s,'t,'c,'d> =
           ConcreteIso (r.to_ << l.to_, l.from << r.from)

   // Concrete optics are useful for many situations but the boiler plate becomes problematic at scale and this representation limits some compositions between optics

   (* As an example concrete isomorphisms compose cleanly so long as the in bound and out bound values are the same type *)
   let optic1 =
       let l0 : string -> int = (fun (s0:string) -> 0)
       let r0 : int -> string = (fun (g0:int) -> "2")
       // ConcreteIso((string -> int), (int -> string))
       ConcreteIso (l0,r0)

   let optic2 =
       let l1 : int -> string = (fun (s1:int) -> "")
       let r1 : string -> int = (fun (g1:string) -> 3)
       //ConcreteIso((int -> string), (string -> int))
       ConcreteIso (l1,r1)

   //l1 :: (string -> int) * (int -> string)
   //r1 :: (int -> string) * (string -> int)
   let optic2 = ConcreteIso ((fun (s1:int) -> ""), (fun (g1:string) -> 3))

   let comp1 = optic1.Compose(optic2)
   let comp2 = optic2.Compose(optic1)

    // forall p . (C0 p, ..., CN p) => p a b -> p s t
   type Optic<'p,'s,'t,'a,'b when 'p :> ProFunctor<'p>> = H2<'p,'a,'b> * H2<'p,'s,'t>
   type Isomorphism<'s,'t,'a,'b>  = Optic<IsoEncoding,'s,'t,'a,'b> //(H<IsoEncoding<'a>,'b> * H<IsoEncoding<'s>,'t>)
   and IsoEncoding() =
      inherit ProFunctor<IsoEncoding>() with
          override __.dimap<'a,'b,'c,'d> (f: 'a -> 'b) (g: 'c -> 'd) c = //(c: H2<IsoEncoding<'p>,'b,'c>) =
              let raw = (c :?> IsoData<'p,'a,'b,'c,'d>)
              let ina f g =
                  let z1 (Isomorphism o,i) = __.dimap (o << f) (g << i)
                  Isomorphism (z1 raw)

              Isomorphism (ina f g)
      end

   and IsoData<'p,'s,'t,'a,'b when 'p :> ProFunctor<'p>> =
      | Isomorphism of (H2<IsoEncoding,'s,'a> -> H2<IsoEncoding,'b,'t>) interface
              //Isomorphism((rmap (o << f)),(lmap (g << i)))
  //    | Isomorphism of (H<'p,'s> -> H<'p,'a>) * ('b -> 't) interface Isomorphism<'p,'s,'t,'a,'b>

    (*type either<'a,'b> = H2<EitherBifunctorEncoding,'a,'b>
and EitherBifunctorEncoding() =
        interface BiFunctor<EitherBifunctorEncoding> with
            member _.bimap(a: 'a -> 'b)(b: 'c -> 'd)(c: H2<EitherBifunctorEncoding,'a,'c>) : H2<EitherBifunctorEncoding,'b,'d> =
                match c :?> eitherData<'a,'c> with
                |  Left l  -> Left (a l) :> _
                |  Right r -> Right (b r) :> _

and eitherData<'a,'b> =
    | Left of 'a
    | Right of 'b
    interface either<'a,'b>

let Left<'a,'b> (e: 'a) : either<'a,'b> = Left e :> _
let Right<'a,'b> (a: 'b ) : either<'a,'b> = Right a :> _
let (|Left|Right|) (m: either<'e, 'a>) =
    match m :?> eitherData<'e, 'a> with
    | Left l  -> Left l
    | Right r -> Right r*)
(*              let z = match n with Star s -> s
                Star (fmap b << z << a ) :> Star<'f,'a,'d>
                (dimap a b c) :> Isomorphism<'p,'s,'t,'a,'b>*)
            //member __.lmap<'a,'b,'c>     (a: 'a -> 'b) (b:H2<IsoEncoding<'p,'a,'d>,'b,'c>) = dimap a id b
            //member __.rmap<'a,'b,'c>     (a: 'b -> 'c) (b:H2<IsoEncoding<'p,'c,'d>,'a,'b>) = dimap id a b
//        end

(*    let shift (Isomorphism (f,g)) x =
        match x with
        | (a,b),c -> f (a,(b,c))
        | a,(b,c) -> g ((a,b),c)*)



//    let shift' (iso:Isomorphism<'p,'s,'t,'a,'b>) =
    (* shift' :: AdapterP ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
       shift' = dimap assoc assoc' where
       assoc  ((x, y), z) = (x, (y, z))
       assoc' (x, (y, z)) = ((x, y), z)
*)

(*    type AdapterP<'p,'s,'t,'a,'b when 'p :> ProFunctor<'p>> =
        interface ProFunctor<'p> with*)
(*

    type Optic'<'p,'s,'a> = Optic<'p,'s,'s,'a,'a>
*)

    //Equality
(*    type AdapterP<'p> = H2<AdapterP<'p>,'s,'t,'a,'b> -> H2<'p,'s,'t>
    type AdapterP<'p,'s,'t,'a,'b when 'p :> ProFunctor<'p>>() =*)

        (*type Star<'f,'d,'c when 'f :> Functor<'f>> = H2<StarEncoding<'f>,'d,'c>
and StarEncoding<'f when 'f :> Functor<'f>>() =
    interface ProFunctor<StarEncoding<'f>> with*)

(*    inherit Functor<maybeBy<'F>>()
    override __.fmap<'a,'b> (a:'a->'b) (b:H<maybeBy<'F>,'a>) =
        match unwrap b  with
        | Some n -> Just (a n)
        | None -> Nothing*)

    //Iso
    //type Iso<'P,'s,'t,'a,'b when 'P :> ProFunctor<'P>> = Optic<'P,'s,'t,'a,'b>
    //type Iso'<'P,'s,'a when 'P :> ProFunctor<'P>> = Optic'<'P,'s,'a>
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