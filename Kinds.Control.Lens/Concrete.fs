namespace Kinds.Control.Lens
open Kinds.Data.Either

module Concrete =
(*=========================================================================================================================================*)
                                                    (*-- Types ---*)
(*=========================================================================================================================================*)
    type Adapter<'s,'t,'a,'b> = Adapter of ('s -> 'a) * ('b -> 't) with
        member this.to_  = match this with Adapter(to_,_) -> to_
        member this.from = match this with Adapter(_,from) -> from
        member l.Compose (r: Adapter<'a,'b,'c,'d>) :  Adapter<'s,'t,'c,'d>=
            Adapter (l.to_ >> r.to_, r.from >> l.from)

    type Lens<'s,'t,'a,'b> = Lens of ('s -> 'a) * ('s -> 'b -> 't) with
        member this.get = match this with Lens(get,_) -> get
        member this.set = match this with Lens(_,set) -> set
        member l.Compose (r:Lens<'a, 'b, 'c, 'd>) : Lens<'s,'t,'c,'d> =
            Lens((l.get >> r.get),(fun s b -> l.set s (r.set (l.get s) b)))

    type Prism<'s,'t,'a,'b> = Prism of ('b -> 't) * ('s -> Either<'t,'a>) with
        member this.embed  = match this with Prism(embed,_) -> embed
        member this.match_ = match this with Prism(_,match_) -> match_
        member l.Compose (r: Prism<'a,'b,'c,'d>) : Prism<'s,'t,'c,'d> =
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

    // A Traversal focuses on zero or more elements of a structure.
    //
    //   contents : 's -> 'a seq        — extract all focused values
    //   fill     : 'b seq -> 's -> 't  — rebuild the structure with new values
    //
    // Composition distributes the replacement sequence across nested structures,
    // "chunking" it according to how many elements each sub-traversal focuses on.
    [<Experimental("Concrete traversal — composition uses eager seq evaluation")>]
    type Traversal<'s,'t,'a,'b> = Traversal of ('s -> 'a seq) * ('b seq -> 's -> 't) with
        member this.contents = match this with Traversal(contents,_) -> contents
        member this.fill     = match this with Traversal(_,fill)     -> fill
        member l.Compose (r: Traversal<'a,'b,'c,'d>) : Traversal<'s,'t,'c,'d> =
            Traversal(
                (fun s -> l.contents s |> Seq.collect r.contents),
                (fun (ds: 'd seq) (s: 's) ->
                    let mutable remaining = Seq.toList ds
                    let bs =
                        l.contents s
                        |> Seq.map (fun a ->
                            let count   = r.contents a |> Seq.length
                            let chunk, rest = List.splitAt count remaining
                            remaining <- rest
                            r.fill (Seq.ofList chunk) a)
                    l.fill bs s))

    // A Grate<'s,'t,'a,'b> focuses via a "coexponential" — a structure that
    // can be observed through any function ('s -> 'a) to produce a 'b,
    // and then reconstructed into a 't.
    //
    //   runGrate : ((s -> a) -> b) -> t
    //
    // Unlike Lens (total, reads one 'a) or Prism (partial, reads zero or one 'a),
    // a Grate distributes across the structure: the extractor (s -> a) can observe
    // the 'a at any point relative to a specific 's.
    //
    // Examples: data indexed by a type parameter, newtype wrappers, products of
    // identical types. The canonical example: Grate (fun f -> f fst, f snd)
    // for pairs (a, a).
    type Grate<'s,'t,'a,'b> = Grate of ((('s -> 'a) -> 'b) -> 't) with
        member this.runGrate = match this with Grate f -> f
        member l.Compose (r: Grate<'a,'b,'c,'d>) : Grate<'s,'t,'c,'d> =
            // (l >=> r).runGrate f
            //   = l.runGrate (fun sa -> r.runGrate (fun ac -> f (ac << sa)))
            Grate (fun (f: ('s -> 'c) -> 'd) ->
                l.runGrate (fun (sa: 's -> 'a) ->
                    r.runGrate (fun (ac: 'a -> 'c) ->
                        f (ac << sa))))

    // An AlgebraicLens<'s,'t,'a,'b> is a Lens extended with a classification
    // algebra — a contextual, partial setter that has access to a training set.
    //
    //   view     : 's -> 'a
    //   classify : 's list -> 'b -> 't option
    //
    // view extracts the focus in the standard way.  classify takes a list of
    // source values (the "training context"), a new focus value 'b, and
    // optionally reconstructs the target 't.  Returning None means the new
    // focus is outside the domain of the classifier.
    //
    // The canonical example is nearest-neighbour classification: given a
    // labelled training set and a new measurement, return the labelled sample
    // whose stored measurement is closest to the new value.
    //
    // Reference: algebraic / para-lens construction.
    type AlgebraicLens<'s,'t,'a,'b> = AlgebraicLens of ('s -> 'a) * ('s list -> 'b -> 't option) with
        member this.view     = match this with AlgebraicLens(v, _) -> v
        member this.classify = match this with AlgebraicLens(_, c) -> c

(*=========================================================================================================================================*)
                                                    (*-- Conversions ---*)
(*=========================================================================================================================================*)
    type Adapter<'s,'t,'a,'b> with
        member l.asLens  = Lens(l.to_, fun _ -> l.from)
        member l.asPrism = Prism(l.from, fun a -> Right (l.to_ a))

    type Lens<'s,'t,'a,'b> with
        member lhs.asAffine    = Affine(fun s -> (Right (lhs.get s), lhs.set s))
        member lhs.asTraversal = Traversal(
            (fun s -> Seq.singleton (lhs.get s)),
            (fun bs s -> lhs.set s (Seq.head bs)))

    type Prism<'s,'t,'a,'b> with
        member lhs.asAffine    = Affine(fun s -> (lhs.match_ s, lhs.embed))
        member lhs.asTraversal = Traversal(
            (fun s -> match lhs.match_ s with Right a -> Seq.singleton a | Left _ -> Seq.empty),
            (fun bs s ->
                match lhs.match_ s with
                | Right _ -> lhs.embed (Seq.head bs)
                | Left  t -> t))

    type Adapter<'s,'t,'a,'b> with
        member l.asAffine    = l.asLens.asAffine
        member l.asTraversal = l.asLens.asTraversal

    //Type Preserving Aliases
    type Iso<'s,'a>            = Adapter<'s,'s,'a,'a>
    type Prism<'s,'a>          = Prism<'s,'s,'a,'a>
    type Lens<'s,'a>           = Lens<'s,'s,'a,'a>
    type Traversal<'s,'a>      = Traversal<'s,'s,'a,'a>
    type Grate<'s,'a>          = Grate<'s,'s,'a,'a>
    type AlgebraicLens<'s,'a>  = AlgebraicLens<'s,'s,'a,'a>

(*=========================================================================================================================================*)
                                                     (*-- Traversal utilities ---*)
(*=========================================================================================================================================*)
    module Traversal =
        /// Extract all focused values as a list.
        let toList (t: Traversal<'s,'s,'a,'a>) (s: 's) : 'a list =
            t.contents s |> Seq.toList

        /// Apply a function to every focused element.
        let modify (t: Traversal<'s,'t,'a,'b>) (f: 'a -> 'b) (s: 's) : 't =
            t.fill (t.contents s |> Seq.map f) s

        /// Replace every focused element with a constant value.
        let set (t: Traversal<'s,'t,'a,'b>) (b: 'b) (s: 's) : 't =
            t.fill (t.contents s |> Seq.map (fun _ -> b)) s

        /// Count the number of focused elements.
        let length (t: Traversal<'s,'s,'a,'a>) (s: 's) : int =
            t.contents s |> Seq.length

(*=========================================================================================================================================*)
                                                     (*-- Grate utilities ---*)
(*=========================================================================================================================================*)
    module Grate =
        /// Embed a 'b into the structure, using a constant extractor.
        /// Equivalent to review for prisms.
        let review (g: Grate<'s,'t,'a,'b>) (b: 'b) : 't =
            g.runGrate (fun _ -> b)

        /// Zip two structures together via the Grate's coexponential.
        /// Both structures are observed and the results combined with 'f'.
        let zipWith (g: Grate<'s,'t,'a,'b>) (f: 'a -> 'a -> 'b) (s1: 's) (s2: 's) : 't =
            g.runGrate (fun sa -> f (sa s1) (sa s2))

(*=========================================================================================================================================*)
                                                     (*-- AlgebraicLens utilities ---*)
(*=========================================================================================================================================*)
    module AlgebraicLens =
        /// Extract the focused value from a structure.
        let view (l: AlgebraicLens<'s,'t,'a,'b>) (s: 's) : 'a =
            l.view s

        /// Apply the classification algebra: given a training context and a new
        /// focus value, optionally produce a new target structure.
        let classify (l: AlgebraicLens<'s,'t,'a,'b>) (training: 's list) (b: 'b) : 't option =
            l.classify training b

        /// Apply a function to the current focus, then classify the result.
        ///
        ///   over l f training s  =  classify l training (f (view l s))
        let over (l: AlgebraicLens<'s,'t,'a,'b>) (f: 'a -> 'b) (training: 's list) (s: 's) : 't option =
            l.classify training (f (l.view s))

        /// Replace the focus with a constant value, then classify.
        ///
        ///   set l b training  =  classify l training b
        let set (l: AlgebraicLens<'s,'t,'a,'b>) (b: 'b) (training: 's list) : 't option =
            l.classify training b

(*=========================================================================================================================================*)
                                                     (*-- Composition ---*)
(*=========================================================================================================================================*)
    type Adapter<'s,'t,'a,'b> with
        static member (>.>) (l:Adapter<'s,'t,'a,'b>, r:Adapter<'a,'b,'c,'d>) = l.Compose(r)
        static member (>.>) (l:Adapter<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.asLens.Compose(r)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Adapter<'a,'b,'c,'d>) = l.Compose(r.asLens)
        static member (>.>) (l:Adapter<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.asPrism.Compose(r)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Adapter<'a,'b,'c,'d>) = l.Compose(r.asPrism)
        static member (>.>) (l:Adapter<'s,'t,'a,'b>, r:Affine<'a,'b,'c,'d>) = l.asAffine.Compose(r)
        static member (>.>) (l:Affine<'s,'t,'a,'b>, r:Adapter<'a,'b,'c,'d>) = l.Compose(r.asAffine)
        static member (>.>) (l:Adapter<'s,'t,'a,'b>, r:Traversal<'a,'b,'c,'d>) = l.asTraversal.Compose(r)
        static member (>.>) (l:Traversal<'s,'t,'a,'b>, r:Adapter<'a,'b,'c,'d>) = l.Compose(r.asTraversal)

    type Lens<'s,'t,'a,'b> with
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.Compose(r)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.asAffine.Compose(r.asAffine)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.asAffine.Compose(r.asAffine)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Affine<'a,'b,'c,'d>) = l.asAffine.Compose(r)
        static member (>.>) (l:Affine<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.Compose(r.asAffine)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Traversal<'a,'b,'c,'d>) = l.asTraversal.Compose(r)
        static member (>.>) (l:Traversal<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.Compose(r.asTraversal)

    type Prism<'s,'t,'a,'b> with
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.Compose(r)
        static member (>.>) (l:Lens<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.asAffine.Compose(r.asAffine)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Lens<'a,'b,'c,'d>) = l.asAffine.Compose(r.asAffine)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Affine<'a,'b,'c,'d>) = l.asAffine.Compose(r)
        static member (>.>) (l:Affine<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.Compose(r.asAffine)
        static member (>.>) (l:Prism<'s,'t,'a,'b>, r:Traversal<'a,'b,'c,'d>) = l.asTraversal.Compose(r)
        static member (>.>) (l:Traversal<'s,'t,'a,'b>, r:Prism<'a,'b,'c,'d>) = l.Compose(r.asTraversal)

    type Affine<'s,'t,'a,'b> with
        static member (>.>) (l:Affine<'s,'t,'a,'b>, r:Affine<'a,'b,'c,'d>) = l.Compose(r)

    type Traversal<'s,'t,'a,'b> with
        static member (>.>) (l:Traversal<'s,'t,'a,'b>, r:Traversal<'a,'b,'c,'d>) = l.Compose(r)

    type Grate<'s,'t,'a,'b> with
        static member (>.>) (l:Grate<'s,'t,'a,'b>, r:Grate<'a,'b,'c,'d>) = l.Compose(r)

    module Unsound =
        let both<'s, 'a, 'b> (la: Lens<'s, 'a>) (lb: Lens<'s, 'b>) =
            Lens((fun s -> (la.get s, lb.get s)), (fun s a -> lb.set (la.set s (fst a)) (snd a)))
