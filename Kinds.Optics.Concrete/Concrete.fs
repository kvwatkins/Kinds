namespace Kinds.Optics
open Kinds.Prelude

module Concrete =
(*=========================================================================================================================================*)
                                                    (*-- Types ---*)
(*=========================================================================================================================================*)
    type Iso<'s,'t,'a,'b> = Iso of ('s -> 'a) * ('b -> 't) with
        member this.to_  = match this with Iso(to_,_) -> to_
        member this.from = match this with Iso(_,from) -> from
        member l.Compose (r: Iso<'a,'b,'c,'d>) :  Iso<'s,'t,'c,'d>=
            Iso (l.to_ >> r.to_, r.from >> l.from)

    type Lens<'s,'t,'a,'b> = Lens of ('s -> 'a) * ('s -> 'b -> 't) with
        member this.get = match this with Lens(get,_) -> get
        member this.set = match this with Lens(_,set) -> set
        member l.Compose (r:Lens<'a, 'b, 'c, 'd>) : Lens<'s,'t,'c,'d> =
            Lens((l.get >> r.get),(fun s b -> l.set s (r.set (l.get s) b)))

    type Prism<'s,'t,'a,'b> = Prism of ('b -> 't) * ('s -> Either<'t,'a>) with
        member this.embed  = match this with Prism(embed,_) -> embed
        member this.match_ = match this with Prism(_,match_) -> match_
        member l.Compose (r: Prism<'a,'b,'c,'d>) : Prism<'s,'t,'a,'d> =
            Prism (l.embed << r.embed, fun s -> Either.bind (fun a -> Either.mapLeft l.embed (r.match_ a) ) (l.match_ s))

     //--- Kaisk Functor + Initial State give form to an alternative affine
     //--- http://oleg.fi/gists/posts/2017-03-20-affine-traversal.html
    type Affine<'s,'t,'a,'b> = Affine of ('s -> (Either<'t,'a>) * ('b -> 't)) with
         member this.affine  = match this with Affine(affine) -> affine
         member l.Compose (r:Affine<'a,'b,'c,'d>)  =
            fun s ->
                 let ta, bt = l.affine s
                 let tx = Either.bind (fun a -> Either.mapLeft bt (fst (r.affine a))) ta
                 let yt = (fun y -> Either.fold id (fun a -> bt ((snd (r.affine a)) y)) ta)
                 Affine (fun f -> tx ,yt)

    (* - Traversals are tricky requiring indirect representation to pull off we will leave this private for now - *)
    [<Experimental("Concrete traversal for internal experimentation only")>]
    type private Traversal<'s,'t,'a,'b> = Traversal of ('s -> 'a seq) * ('b seq -> 's -> 't) with
        member this.contents = match this with Traversal(contents,_) -> contents
        member this.fill     = match this with Traversal(_,fill)     -> fill

(*=========================================================================================================================================*)
                                                    (*-- Conversions ---*)
(*=========================================================================================================================================*)
    type Iso<'s,'t,'a,'b> with
        member l.asLens =  Lens(l.to_, fun _ -> l.from)
        member l.asPrism = Prism(l.from, fun a -> Right (l.to_ a))

    type Lens<'s,'t,'a,'b> with
        member lhs.asAffine = Affine(fun s -> (Right (lhs.get s), lhs.set s))

    type Prism<'s,'t,'a,'b> with
        member lhs.asAffine = Affine( fun s -> (lhs.match_ s, lhs.embed))

    type Iso<'s,'t,'a,'b> with
        member l.asAffine = l.asLens.asAffine

    //Type Preserving Aliases
    type Iso<'s,'a>    = Iso<'s,'s,'a,'a>
    type Prism<'s,'a>  = Prism<'s,'s,'a,'a>
    type Lens<'s,'a>   = Lens<'s,'s,'a,'a>
 (*=========================================================================================================================================*)
                                                     (*-- Composition ---*)
 (*=========================================================================================================================================*)
    type Iso<'s,'t,'a,'b> with
        static member (>.>) (l:Iso<'s,'t,'a,'b>, r:Iso<'a,'b,'c,'d>) = l.Compose(r)
        static member (>.>) (l:Iso<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.asLens.Compose(r)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Iso<'a,'b,'c,'d>) = l.Compose(r.asLens)
        static member (>.>) (l:Iso<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.asPrism.Compose(r)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Iso<'a,'b,'c,'d>) = l.Compose(r.asPrism)
        static member (>.>) (l:Iso<'s,'t,'a,'b>, r:Affine<'a,'b,'c,'d>) = l.asAffine.Compose(r)
        static member (>.>) (l:Affine<'s,'t,'a,'b>, r:Iso<'a,'b,'c,'d>) = l.Compose(r.asAffine)

    type Lens<'s,'t,'a,'b> with
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.Compose(r)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.asAffine.Compose(r.asAffine)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.asAffine.Compose(r.asAffine)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Affine<'a,'b,'c,'d>) = l.asAffine.Compose(r)
        static member (>.>) (l:Affine<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.Compose(r.asAffine)

    type Prism<'s,'t,'a,'b> with
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.Compose(r)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.asAffine.Compose(r.asAffine)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.asAffine.Compose(r.asAffine)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Affine<'a,'b,'c,'d>) = l.asAffine.Compose(r)
        static member (>.>) (l:Affine<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.Compose(r.asAffine)

    type Affine<'s,'t,'a,'b> with
        static member (>.>) (l:Affine<'s,'t,'a,'b>, r:Affine<'a,'b,'c,'d>) = l.Compose(r)

    module Unsound =
        let both<'s, 'a, 'b> (la: Lens<'s, 'a>) (lb: Lens<'s, 'b>) =
            Lens((fun s -> (la.get s, lb.get s)), (fun s a -> lb.set (la.set s (fst a)) (snd a)))
