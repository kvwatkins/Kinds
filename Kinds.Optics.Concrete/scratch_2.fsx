(*=========================================================================================================================================*)
                                                    (*--- Optics ---*)
(*
                                        Category Theory is the Essence of Composition
                                            Optics is the Essence of Decomposition
*)
(*=========================================================================================================================================*)

(* Why is this important:
    Optics are important they are defined by laws, they are composable, and they represent the dual of composition, decomposition.

    What we'll do:
    We'll start by taking a look at functor optics, which are reification of the concept of object composition through decomposition.
    we decompose an object 's into a focus 'a and the residual/residue C and then constructing a new object of 't using 'b.
    Optics is still being researched so it comes in many forms. The leading variant described

    The Residue can be thought of as the contextual difference or idea of whats left over after decomposing an object.
    Unfortunately decomposition cant be defined as uniformly as composition furthermore it requires a language to implement more advanced
    typing systems to be supported. We can get around this limitation of f# in a number of ways here we will be using reified phantom types.

    Optics each are a pair of functions that represent the decomposition and recomposition process, they are paired with one or more actions
    that when used with the optic and the structure perform the decomposition recomposition process. The key insight here is that optics
    are compatible with each other and can be composed. What makes this incredibly useful is each optic is based in a set
    of mathematical laws that when followed and asserted, allow you to create composed pipelines of safety about your operations on them. *)

(*=========================================================================================================================================*)
(* Prelude *)
(*=========================================================================================================================================*)

type Either<'a, 'b> = Choice<'b,'a>
let Right x :Either<'a,'b> = Choice1Of2 x
let Left x :Either<'a,'b> = Choice2Of2 x
let (|Right|Left|) = function Choice1Of2 x -> Right x | Choice2Of2 x -> Left x
let either f g = function Left x -> f x | Right y -> g y

let bimap (f: 'a -> 'a0) (g: 'b -> 'b0) (e: Either<'a, 'b>) = match e with | Left a -> Left(f a) | Right b -> Right(g b)
let map (f: 'b -> 'a0) (e: Either<'a, 'b>) = bimap id f e
let mapLeft (f: 'a -> 'a0) (e: Either<'a, 'b>) = bimap f id e
let bind (f: 'b -> Either<'a, 'a0>) (e: Either<'a, 'b>) : Either<'a, 'a0> = match e with | Left a -> Left a | Right b -> f b
let right (e: Either<'a, 'b>) = match e with | Left _ -> None | Right b -> Some b
let left (e: Either<'a, 'b>) = match e with | Left a -> Some a | Right _ -> None
let fold (l: 'a -> 'a0) (r: 'b -> 'a0) (e: Either<'a, 'b>) = match e with | Left a -> l a | Right b -> r b

(*=========================================================================================================================================*)
(*  Simple concrete optics

    These optics are type preserving
    types 't and 'b are restricted to the types they started as so s,t,a,b becomes s, s', a, 'a
    The compiler will warns you by flagging the values yellow

    Actual Values::
            type Iso'<'s,'a> = Iso<'s,'s,'a,'a>
            type Lens'<'s,'a> = Lens<'s,'s,'a,'a>
            type Prism'<'s,'a> = Prism<'s,'s,'a,'a> *)
(*=========================================================================================================================================*)
type Lens'<'s,'t,'a,'b> = Lens of ('s -> 'a) * ('s -> 'b -> 't) with
    member this.get = match this with Lens(get,_) -> get
    member this.set = match this with Lens(_,set) -> set

type Iso'<'s,'t,'a,'b> = Iso of ('s -> 'a) * ('b -> 't) with
    member this.to_ = match this with Iso(to_,_) -> to_
    member this.from = match this with Iso(_,from) -> from

type Prism'<'s,'t,'a,'b> = Prism of ('b -> 't) * ('s -> Either<'t,'a>) with
    member this.embed = match this with Prism(embed,_) -> embed
    member this.match_ = match this with Prism(_,match_) -> match_

(*=========================================================================================================================================*)
(*  Polymorphic Phantom Optics
     These optics are are polymorphic due to a reification of a phantom into a const type (Tagged in Haskell), essentially this serves as a wrapper
     to throw away the knowledge of 's and 'a. *)
(*=========================================================================================================================================*)

[<Struct>] type Const<'t,'u> = Const of 't
let getConst (Const t ) = t

type Optic<'p,'s,'t,'a,'b> = Optic of Const<'a -> 'b,'p> * Constype Affine<'s,'t,'a,'b> = Affine of (   witha) -> '*()TraversalTraversal'[ ' -> 's // Travers  with   setsetAffineset   (* &*) ;';;;;  ''''''t<'s -> 't,'p>

(*=========================================================================================================================================*)
(* Iso |||> Coercion Adapters Categorical Isomorphisms Identity Preserving Map *)
(*=========================================================================================================================================*)

type Iso<'p,'s,'t,'a,'b> = Iso of Const<'s -> 'a,'p> * Const<'b -> 't,'p> with
        member inline this.to_ = match this with Iso(to_,_) -> getConst to_
        member inline this.from = match this with Iso(_,from) -> getConst from

//Iso Functions
let inline iso to_ from = Iso(Const(to_), Const(from))
let inline withIso (iso:Iso<'p,'s,'t,'a,'b>) = iso.to_, iso.from
let inline from (i:Iso<'p,'s,'t,'a,'b>) = (withIso i)
                                          |> fun i' -> fst i', snd i'
                                          |> fun (sa,bt) -> (iso bt sa) : Iso<'p,'b,'a,'t,'s>
(*=========================================================================================================================================*)
(* Lens |||> Product Types Disjoint Getters/Accessors *)
(*=========================================================================================================================================*)
type Lens<'p,'s,'t,'a,'b> = Lens of Const<'s -> 'a,'p> * Const<'s -> 'b -> 't,'p> with
        member inline this.get = match this with Lens(get, _) -> getConst get
        member inline this.set = match this with Lens(_,set) -> getConst set

//Lens Functions
let inline lens (get:'s->'a) (set: 's->'b->'t) = Lens (Const(get), Const(set))
let inline view (lens:Lens<'p,'s,'t,'a,'b>) (s:'s) = lens.get s
let inline set (lens:Lens<'p,'s,'t,'a,'b>) (s:'s) b =lens.set s b
let inline over (l:Lens<'p,'s,'t,'a,'b>) s (f: 'a -> 'b) : 't = s |> (l.get >> f >> l.set s)

(*=========================================================================================================================================*)
(* Conversions *)
(*=========================================================================================================================================*)
type Iso<'p,'s,'t,'a,'b> with member this.toLens : Lens<'p,_,_,_,_> = lens this.to_ (fun _-> this.from)
(*=========================================================================================================================================*)
(* Composition *)
(*=========================================================================================================================================*)
//Homogenous Composition
type Iso<'p,'s,'t,'a,'b> with
    static member inline (>.>) (o1, o2) =
            let (Iso (x1, x2)) = o1
            let (Iso (y1, y2)) = o2
            let x1Con,x2Con = (getConst x1),(getConst x2)
            let y1Con,y2Con = (getConst y1),(getConst y2)
            Iso (Const(x1Con >> x2Con), Const(y1Con >> y2Con))

type Lens<'p,'s,'t,'a,'b> with
    static member (>.>) (lens1,lens2) =
                let (Lens (get1, set1)) = lens1
                let (Lens (get2, set2)) = lens2
                let getter1,getter2 = (getConst get1),(getConst get2)
                let setter1,setter2 = (getConst set1),(getConst set2)
                Lens(Const(getter1 >> getter2), (Const(fun s b -> setter1 s (setter2 (getter1 s) b))))

//Heterogeneous Composition
type Iso<'p,'s,'t,'a,'b> with
    static member inline (>.>) (l:Iso<'p,'s,'t,'a,'b>, r) = l.toLens >.> r
(*=========================================================================================================================================*)
(* Examples *)
(*=========================================================================================================================================*)
//Lens
[<Struct>] type StreetAddress = {name: string; streetNumber: int}
[<Struct>] type Person = {name: string; Address: StreetAddress; }
let getStreetAddress = Const (fun (person: Person) -> person.Address)
let setStreetAddress = (Const(fun (person: Person) (address: StreetAddress) -> { name = person.name; Address = address }))
let getStreetNumber = Const(fun (streetAddress: StreetAddress) -> streetAddress.streetNumber)
let setStreetNumber = Const(fun (streetAddress: StreetAddress) number -> {streetAddress with streetNumber = number})

let streetAddress = Lens (getStreetAddress,setStreetAddress)
let streetNumber = Lens (getStreetNumber,setStreetNumber)
let getPersonsStreetNumber = view (streetAddress >.> streetNumber)
let setPersonsStreetNumber = set (streetAddress >.> streetNumber)
let updateOverStreetNumber = over (streetAddress >.> streetNumber)
let personWithAddress = {name = "Kenny"; Address = {name = "test001"; streetNumber = 10;}}
let a = view (streetAddress >.> streetNumber) personWithAddress
let b = set (streetAddress >.> streetNumber) personWithAddress 15
let c = over (streetAddress >.> streetNumber) personWithAddress (fun f -> f + 3)

a
b |> view (streetAddress >.> streetNumber)
c |> view (streetAddress >.> streetNumber)


//ISO
//Creating the optic
let fromTuple = (fun (_:'t) -> if true then Some(1) else None)
let toTuple = (fun x -> match x with | Some (_:'t) -> 1 | None -> -1)
let isoTupleOption = Iso(Const(toTuple), Const(fromTuple))

let fromOptTuple x = x |> (snd (withIso isoTupleOption))
let toOptTuple x = x |> (snd (withIso (from isoTupleOption)))

(* //Prism Functions
let prism (embed:'b -> 't) (match_: 's -> Either<'t,'a>) = Prism (embed, match_) : Prism<'s,'t,'a,'b>
let preview (prism: Prism<'s,'t,'a,'b>) (s: 's) = right (prism.match_ s)
let review (prism: Prism<'s,'t,'a,'b>) (a: 'b) = prism.embed a*)