namespace Kinds.Control.Lens.Profunctor

open Encoding
open TypeClass
open TypeClass.ProFunctor
open Kinds.Data.Either   // either, Either<_,_>, Right, Left, Either.fold, Either.right
open Kinds.Data.Identity // IdentityEncoding, runIdentity

/// Profunctor optics: a uniform interface for Iso, Lens, Prism, and more.
///
/// The central idea: an optic is a rank-2 function
///
///   type POptic p s t a b = p a b -> p s t   (for some profunctor constraint on p)
///
/// Different profunctor constraints characterise different optic classes:
///
///   Iso   : ProFunctor p              — dimap alone suffices
///   Lens  : Cartesian  p              — thread product types (×) through p
///   Prism : CoCartesian p             — thread sum types (+) through p
///
/// Operations are recovered by instantiating p with a chosen profunctor:
///
///   view    — p = Forget r        (extract the focused value)
///   over    — p = Star Identity   (map over the focused value)
///   review  — p = Tagged          (embed a value into the structure)
module Profunctor =

(*=========================================================================*)
(*-- Forget — the "getter" profunctor ------------------------------------*)
(*=========================================================================*)

    // Forget<'r,'a,'b> wraps 'a -> 'r, ignoring the phantom output type 'b.
    //
    //   dimap f _ (Forget k) = Forget (k << f)   — the output mapping is discarded
    //
    // Why? Forget only ever reads from the structure; it can never produce new output.
    // This makes it perfect for `view`: substituting Forget id into a lens optic
    // extracts exactly the focused value.
    //
    // Cartesian instance:
    //   first  (Forget k) = Forget (k << fst)
    //   second (Forget k) = Forget (k << snd)
    type Forget<'r,'a,'b> = Forget of ('a -> 'r) interface H2<ForgetEncoding<'r>,'a,'b>
    and ForgetEncoding<'r>() =
        inherit ProFunctor<ForgetEncoding<'r>>()

        override __.dimap<'a,'b,'c,'d> (f: 'a -> 'b) (_: 'c -> 'd) (pbc: H2<ForgetEncoding<'r>,'b,'c>) =
            let (Forget k) = pbc :?> Forget<'r,'b,'c>
            Forget (k << f) :> H2<ForgetEncoding<'r>,'a,'d>

        interface Cartesian<ForgetEncoding<'r>> with
            member __.first<'a,'b,'c> (pab: H2<ForgetEncoding<'r>,'a,'b>) =
                let (Forget k) = pab :?> Forget<'r,'a,'b>
                Forget (fun (a, _) -> k a) :> H2<ForgetEncoding<'r>,'a*'c,'b*'c>
            member __.second<'a,'b,'c> (pab: H2<ForgetEncoding<'r>,'a,'b>) =
                let (Forget k) = pab :?> Forget<'r,'a,'b>
                Forget (fun (_, a) -> k a) :> H2<ForgetEncoding<'r>,'c*'a,'c*'b>

    /// Extract the getter function from a Forget value.
    let runForget<'r,'a,'b> (h: H2<ForgetEncoding<'r>,'a,'b>) : 'a -> 'r =
        let (Forget k) = h :?> Forget<'r,'a,'b> in k

(*=========================================================================*)
(*-- Tagged — the "constructor" profunctor --------------------------------*)
(*=========================================================================*)

    // Tagged<'a,'b> wraps a 'b, ignoring the phantom input type 'a.
    //
    //   dimap _ g (Tagged b) = Tagged (g b)   — the input mapping is discarded
    //
    // Why? Tagged only ever produces output; it has no input to consume.
    // This makes it perfect for `review`: substituting Tagged b into a prism optic
    // constructs a structure containing b.
    //
    // CoCartesian instance:
    //   left_  (Tagged b) = Tagged (Left  b)
    //   right_ (Tagged b) = Tagged (Right b)
    type Tagged<'a,'b> = Tagged of 'b interface H2<TaggedEncoding,'a,'b>
    and TaggedEncoding() =
        inherit ProFunctor<TaggedEncoding>()

        override __.dimap<'a,'b,'c,'d> (_: 'a -> 'b) (g: 'c -> 'd) (pbc: H2<TaggedEncoding,'b,'c>) =
            let (Tagged b) = pbc :?> Tagged<'b,'c>
            Tagged (g b) :> H2<TaggedEncoding,'a,'d>

        interface CoCartesian<TaggedEncoding> with
            member __.left_<'a,'b,'c> (pab: H2<TaggedEncoding,'a,'b>) =
                let (Tagged b) = pab :?> Tagged<'a,'b>
                Tagged (Left b) :> H2<TaggedEncoding, Either<'a,'c>, Either<'b,'c>>
            member __.right_<'a,'b,'c> (pab: H2<TaggedEncoding,'a,'b>) =
                let (Tagged b) = pab :?> Tagged<'a,'b>
                Tagged (Right b) :> H2<TaggedEncoding, Either<'c,'a>, Either<'c,'b>>

        // Closed instance: a Tagged value ignores its input, so threading through
        // a function space simply wraps the constant value in a function that
        // ignores its argument.
        //
        //   closed (Tagged b) = Tagged (fun _ -> b)
        interface Closed<TaggedEncoding> with
            member __.closed<'x,'a,'b> (pab: H2<TaggedEncoding,'a,'b>) =
                let (Tagged b) = pab :?> Tagged<'a,'b>
                Tagged (fun (_: 'x) -> b) :> H2<TaggedEncoding,'x->'a,'x->'b>

    /// Extract the wrapped value from a Tagged value.
    let runTagged<'a,'b> (h: H2<TaggedEncoding,'a,'b>) : 'b =
        let (Tagged b) = h :?> Tagged<'a,'b> in b

(*=========================================================================*)
(*-- Optic type alias -----------------------------------------------------*)
(*=========================================================================*)

    /// A profunctor optic: a rank-2 function that works for any profunctor p.
    ///
    /// In Haskell this is expressed with RankNTypes:
    ///   type Optic p s t a b = forall p. C p => p a b -> p s t
    ///
    /// In F# we fix the profunctor 'p at each use site.
    type POptic<'p,'s,'t,'a,'b when 'p :> ProFunctor<'p>> =
        H2<'p,'a,'b> -> H2<'p,'s,'t>

(*=========================================================================*)
(*-- Exchange — the "iso-matcher" profunctor ------------------------------*)
(*=========================================================================*)

    // Exchange<'a,'b,'s,'t> records both directions of an isomorphism:
    //   sa : 's -> 'a   (forward / getter)
    //   bt : 'b -> 't   (backward / setter)
    //
    // Substituting Exchange into a POptic characterised by ProFunctor recovers
    // the iso's component functions without having to inspect implementation.
    //
    //   dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
    type Exchange<'a,'b,'s,'t> =
        | Exchange of ('s -> 'a) * ('b -> 't) interface H2<ExchangeEncoding<'a,'b>,'s,'t>
    and ExchangeEncoding<'a,'b>() =
        inherit ProFunctor<ExchangeEncoding<'a,'b>>()

        override __.dimap<'s,'t,'u,'v> (f: 's -> 't) (g: 'u -> 'v) (pbc: H2<ExchangeEncoding<'a,'b>,'t,'u>) =
            let (Exchange(sa, bt)) = pbc :?> Exchange<'a,'b,'t,'u>
            Exchange(sa << f, g << bt) :> H2<ExchangeEncoding<'a,'b>,'s,'v>

(*=========================================================================*)
(*-- Market — the "prism-matcher" profunctor ------------------------------*)
(*=========================================================================*)

    // Market<'a,'b,'s,'t> records both components of a prism:
    //   bt   : 'b -> 't              (embed — construct 't from 'b)
    //   sEta : 's -> Either<'t,'a>   (match — extract 'a or pass 't through)
    //
    // Substituting Market into a CoCartesian POptic lets us implement a fully
    // profunctor-polymorphic `preview` without touching the concrete Prism.
    //
    //   dimap f g (Market bt sEta) =
    //     Market (g . bt) (bimap g id . sEta . f)
    type Market<'a,'b,'s,'t> =
        | Market of ('b -> 't) * ('s -> Either<'t,'a>) interface H2<MarketEncoding<'a,'b>,'s,'t>
    and MarketEncoding<'a,'b>() =
        inherit ProFunctor<MarketEncoding<'a,'b>>()

        override __.dimap<'s,'t,'u,'v> (f: 's -> 't) (g: 'u -> 'v) (pbc: H2<MarketEncoding<'a,'b>,'t,'u>) =
            let (Market(bt, tEta)) = pbc :?> Market<'a,'b,'t,'u>
            Market(
                g << bt,
                fun s ->
                    match tEta (f s) with
                    | Left t  -> Left (g t)
                    | Right a -> Right a)
            :> H2<MarketEncoding<'a,'b>,'s,'v>

        // CoCartesian: thread sum types through a Market.
        //
        // right_ (Market bt sEta) :: Market a b (Either c s) (Either c t)
        //   bt'   b = Right (bt b)
        //   sEta' (Left  c) = Left (Left c)     — no focus, pass left branch through
        //   sEta' (Right s) = mapLeft Right (sEta s)  — try matching on s
        interface CoCartesian<MarketEncoding<'a,'b>> with
            member __.right_<'s,'t,'c> (pab: H2<MarketEncoding<'a,'b>,'s,'t>) =
                let (Market(bt, sEta)) = pab :?> Market<'a,'b,'s,'t>
                Market(
                    Right << bt,
                    fun e ->
                        match e with
                        | Left  c -> Left (Left c)
                        | Right s -> Either.mapLeft Right (sEta s))
                :> H2<MarketEncoding<'a,'b>, Either<'c,'s>, Either<'c,'t>>

            member __.left_<'s,'t,'c> (pab: H2<MarketEncoding<'a,'b>,'s,'t>) =
                let (Market(bt, sEta)) = pab :?> Market<'a,'b,'s,'t>
                Market(
                    Left << bt,
                    fun e ->
                        match e with
                        | Right c -> Left (Right c)
                        | Left  s -> Either.mapLeft Left (sEta s))
                :> H2<MarketEncoding<'a,'b>, Either<'s,'c>, Either<'t,'c>>

(*=========================================================================*)
(*-- ForgetList — the "fold" profunctor -----------------------------------*)
(*=========================================================================*)

    // ForgetList<'r,'a,'b> wraps a function 'a -> 'r list, ignoring the phantom
    // output type 'b. The list is accumulated across the structure.
    //
    //   dimap f _ (ForgetList k) = ForgetList (k . f)   — output ignored
    //
    // Cartesian instance accumulates from the focused first/second component.
    // CoCartesian instance returns [] for the branch that does not match,
    // enabling fold over optional foci (prisms, affine traversals).
    //
    // Substituting ForgetList (fun a -> [a]) into any optic implements `toListOf`.
    type ForgetList<'r,'a,'b> =
        | ForgetList of ('a -> 'r list) interface H2<ForgetListEncoding<'r>,'a,'b>
    and ForgetListEncoding<'r>() =
        inherit ProFunctor<ForgetListEncoding<'r>>()

        override __.dimap<'a,'b,'c,'d> (f: 'a -> 'b) (_: 'c -> 'd) (pbc: H2<ForgetListEncoding<'r>,'b,'c>) =
            let (ForgetList k) = pbc :?> ForgetList<'r,'b,'c>
            ForgetList (k << f) :> H2<ForgetListEncoding<'r>,'a,'d>

        interface Cartesian<ForgetListEncoding<'r>> with
            member __.first<'a,'b,'c> (pab: H2<ForgetListEncoding<'r>,'a,'b>) =
                let (ForgetList k) = pab :?> ForgetList<'r,'a,'b>
                ForgetList (fun (a, _) -> k a) :> H2<ForgetListEncoding<'r>,'a*'c,'b*'c>
            member __.second<'a,'b,'c> (pab: H2<ForgetListEncoding<'r>,'a,'b>) =
                let (ForgetList k) = pab :?> ForgetList<'r,'a,'b>
                ForgetList (fun (_, a) -> k a) :> H2<ForgetListEncoding<'r>,'c*'a,'c*'b>

        // CoCartesian: return [] for branches that do not contain the focus.
        interface CoCartesian<ForgetListEncoding<'r>> with
            member __.left_<'a,'b,'c> (pab: H2<ForgetListEncoding<'r>,'a,'b>) =
                let (ForgetList k) = pab :?> ForgetList<'r,'a,'b>
                ForgetList (fun e -> match e with Left a -> k a | Right _ -> [])
                :> H2<ForgetListEncoding<'r>, Either<'a,'c>, Either<'b,'c>>
            member __.right_<'a,'b,'c> (pab: H2<ForgetListEncoding<'r>,'a,'b>) =
                let (ForgetList k) = pab :?> ForgetList<'r,'a,'b>
                ForgetList (fun e -> match e with Right a -> k a | Left _ -> [])
                :> H2<ForgetListEncoding<'r>, Either<'c,'a>, Either<'c,'b>>

    /// Extract the accumulation function from a ForgetList.
    let runForgetList<'r,'a,'b> (h: H2<ForgetListEncoding<'r>,'a,'b>) : 'a -> 'r list =
        let (ForgetList k) = h :?> ForgetList<'r,'a,'b> in k

(*=========================================================================*)
(*-- AlgebraicExchange — the "algebraic-lens matcher" profunctor ----------*)
(*=========================================================================*)

    // AlgebraicExchange<'a,'b,'s,'t> records both components of an AlgebraicLens:
    //   view     : 's -> 'a
    //   classify : 's list -> 'b -> 't option
    //
    // Analogous to Exchange<'a,'b,'s,'t> for Iso and Market<'a,'b,'s,'t> for Prism.
    // Substituting AlgebraicExchange into an algebraicLensToP optic recovers both
    // the view getter and the classify algebra without touching the concrete type.
    //
    // dimap f g (AlgebraicExchange(view, cls))
    //   = AlgebraicExchange(view << f, fun ss b -> cls (List.map f ss) b |> Option.map g)
    //
    // f pre-processes the source values (both for view and for the training list);
    // g post-processes the reconstructed target.
    type AlgebraicExchange<'a,'b,'s,'t> =
        | AlgebraicExchange of ('s -> 'a) * ('s list -> 'b -> 't option)
        interface H2<AlgebraicExchangeEncoding<'a,'b>,'s,'t>
    and AlgebraicExchangeEncoding<'a,'b>() =
        inherit ProFunctor<AlgebraicExchangeEncoding<'a,'b>>()

        override __.dimap<'s,'s2,'t,'t2> (f: 's -> 's2) (g: 't -> 't2)
                (p: H2<AlgebraicExchangeEncoding<'a,'b>,'s2,'t>) =
            let (AlgebraicExchange(view, cls)) = p :?> AlgebraicExchange<'a,'b,'s2,'t>
            AlgebraicExchange(
                view << f,
                fun ss b -> cls (List.map f ss) b |> Option.map g)
            :> H2<AlgebraicExchangeEncoding<'a,'b>,'s,'t2>

(*=========================================================================*)
(*-- Concrete → Profunctor conversions ------------------------------------*)
(*=========================================================================*)

    /// Convert a concrete Adapter (Iso) to profunctor form.
    ///
    ///   adapterToP (Adapter to_ from) pab = dimap to_ from pab
    ///
    /// Only ProFunctor is required — `dimap` alone encodes an isomorphism.
    let adapterToP
            (a: Kinds.Control.Lens.Concrete.Adapter<'s,'t,'a,'b>)
            : POptic<'p,'s,'t,'a,'b> =
        dimap a.to_ a.from

    /// Convert a concrete Lens to profunctor form.
    ///
    ///   lensToP (Lens get set) pab = dimap (\s -> (get s, s)) (\(b,s) -> set s b) (first pab)
    ///
    /// Requires Cartesian p (Strong profunctor) for `first`, which threads the
    /// original structure 's through so it is available when rebuilding.
    let lensToP
            (lens: Kinds.Control.Lens.Concrete.Lens<'s,'t,'a,'b>)
            : POptic<'p,'s,'t,'a,'b> =
        fun pab ->
            first<'a,'b,'s,'p> pab
            |> dimap (fun s -> (lens.get s, s)) (fun (b, s) -> lens.set s b)

    /// Convert a concrete Prism to profunctor form.
    ///
    ///   prismToP (Prism embed match_) pab =
    ///     dimap match_ (either id embed) (right_ pab)
    ///
    /// Requires CoCartesian p (Choice profunctor) for `right_`, which routes
    /// the Left branch (already-correct structure) through unchanged.
    let prismToP
            (prism: Kinds.Control.Lens.Concrete.Prism<'s,'t,'a,'b>)
            : POptic<'p,'s,'t,'a,'b> =
        fun pab ->
            right_<'a,'b,'t,'p> pab
            |> dimap prism.match_ (Either.fold id prism.embed)

(*=========================================================================*)
(*-- Optic operations -----------------------------------------------------*)
(*=========================================================================*)

    /// Extract the focused value from a structure.
    ///
    ///   view (lensToP l)   s  =  l.get s
    ///   view (adapterToP a) s  =  a.to_ s
    ///
    /// Works with any POptic whose profunctor is ForgetEncoding<'a>.
    let view (optic: POptic<ForgetEncoding<'a>,'s,'t,'a,'b>) (s: 's) : 'a =
        runForget (optic (Forget id :> H2<ForgetEncoding<'a>,'a,'b>)) s

    /// Apply a function to the focused value, rebuilding the structure.
    ///
    ///   over (lensToP l)   f s  =  l.set s (f (l.get s))
    ///   over (adapterToP a) f s  =  a.from (f (a.to_ s))
    ///
    /// Works with any POptic whose profunctor is StarEncoding<IdentityEncoding>.
    let over
            (optic: POptic<Kinds.Data.Star.StarEncoding<IdentityEncoding>,'s,'t,'a,'b>)
            (f: 'a -> 'b) (s: 's) : 't =
        let star =
            Kinds.Data.Star.Star (fun a -> IdentityEncoding.wrap (f a))
            :> Kinds.Data.Star.Star<IdentityEncoding,'a,'b>
        runIdentity (Kinds.Data.Star.run (optic star) s)

    /// Replace the focused value with a constant.
    ///
    ///   set (lensToP l) b s  =  l.set s b
    let set
            (optic: POptic<Kinds.Data.Star.StarEncoding<IdentityEncoding>,'s,'t,'a,'b>)
            (b: 'b) (s: 's) : 't =
        over optic (fun _ -> b) s

    /// Build a structure from a focused value (for Iso and Prism).
    ///
    ///   review (prismToP p)  b  =  p.embed b
    ///   review (adapterToP a) b  =  a.from b
    ///
    /// Works with any POptic whose profunctor is TaggedEncoding.
    let review (optic: POptic<TaggedEncoding,'s,'t,'a,'b>) (b: 'b) : 't =
        runTagged (optic (Tagged b :> H2<TaggedEncoding,'a,'b>))

    /// Optionally extract the focused value (for Prisms).
    /// Returns None if the prism's match fails.
    ///
    /// This uses the concrete Prism directly; for the profunctor-polymorphic
    /// version that works with any POptic, see `previewP`.
    let preview (prism: Kinds.Control.Lens.Concrete.Prism<'s,'t,'a,'b>) (s: 's) : 'a option =
        Either.right (prism.match_ s)

    /// Convert a concrete Grate to profunctor form.
    ///
    ///   grateToP (Grate g) pab =
    ///     dimap (fun s k -> k s) g (closed pab)
    ///
    /// The key insight: choose x = (s -> a) in `closed`, giving
    ///   closed pab : p ((s->a)->a) ((s->a)->b)
    /// Then:
    ///   pre  : s -> (s->a)->a  =  fun s k -> k s   (flip application)
    ///   post : (s->a)->b -> t  =  g   (the Grate's runGrate)
    ///
    /// Requires Closed p.
    let grateToP
            (g: Kinds.Control.Lens.Concrete.Grate<'s,'t,'a,'b>)
            : POptic<'p,'s,'t,'a,'b> =
        fun (pab: H2<'p,'a,'b>) ->
            // closed with x = (s -> a): gives p ((s->a)->a) ((s->a)->b)
            let cpab = closed<'s->'a,'a,'b,'p> pab
            dimap (fun (s: 's) (k: 's -> 'a) -> k s) g.runGrate cpab

    /// Convert a concrete AlgebraicLens to profunctor form.
    ///
    ///   algebraicLensToP l pab = AlgebraicExchange(l.view, l.classify)
    ///
    /// The input `pab` carries type information only; AlgebraicExchangeEncoding
    /// is a "sink" profunctor that extracts both optic components directly,
    /// analogous to Exchange for Iso.
    let algebraicLensToP
            (l: Kinds.Control.Lens.Concrete.AlgebraicLens<'s,'t,'a,'b>)
            (_pab: H2<AlgebraicExchangeEncoding<'a,'b>,'a,'b>)
            : H2<AlgebraicExchangeEncoding<'a,'b>,'s,'t> =
        AlgebraicExchange(l.view, l.classify) :> _

    /// Convert a concrete Affine (AffineTraversal) to profunctor form.
    ///
    ///   affineTraversalToP (Affine aff) pab =
    ///     dimap aff.affine (fun (etb, bt) -> Either.fold id bt etb)
    ///         (first (right_ pab))
    ///
    /// Step by step:
    ///   right_ pab   : p (Either t a) (Either t b)
    ///   first (...)  : p (Either t a × (b->t)) (Either t b × (b->t))
    ///   dimap        : pre = aff.affine, post = collapse Either t b with b->t
    ///
    /// Requires Cartesian p AND CoCartesian p.
    let affineTraversalToP
            (aff: Kinds.Control.Lens.Concrete.Affine<'s,'t,'a,'b>)
            : POptic<'p,'s,'t,'a,'b> =
        fun (pab: H2<'p,'a,'b>) ->
            right_<'a,'b,'t,'p> pab
            |> first<Either<'t,'a>, Either<'t,'b>, 'b->'t, 'p>
            |> dimap aff.affine (fun (etb, bt) -> Either.fold id bt etb)

    /// Extract the forward and backward functions from an Iso (Adapter) optic.
    ///
    ///   withIso iso k = let sa = iso.to_, bt = iso.from in k sa bt
    ///
    /// Uses Exchange to recover both components without pattern-matching on
    /// the concrete Adapter type.
    let withIso
            (optic: POptic<ExchangeEncoding<'a,'b>,'s,'t,'a,'b>)
            (k: ('s -> 'a) -> ('b -> 't) -> 'r) : 'r =
        let seed : H2<ExchangeEncoding<'a,'b>,'a,'b> =
            Exchange(id, id) :> _
        let (Exchange(sa, bt)) = optic seed :?> Exchange<'a,'b,'s,'t>
        k sa bt

    /// Extract the focused value from an Iso.
    ///
    ///   matching iso s = iso.to_ s
    ///
    /// Works with any POptic whose profunctor is ExchangeEncoding<'a,'b>.
    let matching
            (optic: POptic<ExchangeEncoding<'a,'b>,'s,'t,'a,'b>)
            (s: 's) : 'a =
        let seed : H2<ExchangeEncoding<'a,'b>,'a,'b> =
            Exchange(id, id) :> _
        let (Exchange(sa, _)) = optic seed :?> Exchange<'a,'b,'s,'t>
        sa s

    /// Profunctor-polymorphic preview — optionally extract the focused value.
    ///
    /// Unlike `preview` (which takes a concrete Prism), `previewP` works with
    /// any POptic that is characterised by MarketEncoding. This includes
    /// POptics created via `prismToP`.
    ///
    ///   previewP (prismToP p) s = Either.right (p.match_ s)
    ///
    /// Uses Market to thread the prism's match through without direct
    /// access to the concrete Prism record.
    let previewP
            (optic: POptic<MarketEncoding<'a,'b>,'s,'t,'a,'b>)
            (s: 's) : 'a option =
        let seed : H2<MarketEncoding<'a,'b>,'a,'b> =
            Market(id, Right) :> _
        let (Market(_, sEta)) = optic seed :?> Market<'a,'b,'s,'t>
        Either.right (sEta s)

    /// Collect all focused values into a list.
    ///
    ///   toListOf (lensToP l)    s  =  [l.get s]
    ///   toListOf (prismToP p)   s  =  match p.match_ s with Right a -> [a] | Left _ -> []
    ///
    /// Works with any POptic whose profunctor is ForgetListEncoding<'a>.
    /// Covers lenses, prisms, affine traversals, and traversals that implement
    /// Cartesian + CoCartesian.
    let toListOf
            (optic: POptic<ForgetListEncoding<'a>,'s,'t,'a,'b>)
            (s: 's) : 'a list =
        let seed : H2<ForgetListEncoding<'a>,'a,'b> =
            ForgetList (fun a -> [a]) :> _
        runForgetList (optic seed) s

    /// Zip two structures together using a Grate optic.
    ///
    ///   zipWithGrate (grateToP g) f s1 s2  =  g.runGrate (fun sa -> f (sa s1) (sa s2))
    ///
    /// This is the canonical operation enabled by Grate optics that is not
    /// available for Lens or Prism.
    let zipWithGrate
            (g: Kinds.Control.Lens.Concrete.Grate<'s,'t,'a,'b>)
            (f: 'a -> 'a -> 'b) (s1: 's) (s2: 's) : 't =
        g.runGrate (fun (sa: 's -> 'a) -> f (sa s1) (sa s2))

    /// Extract the view and classify components from a profunctor-encoded
    /// AlgebraicLens.
    ///
    ///   withAlgebraicLens (algebraicLensToP l) k
    ///     = k l.view l.classify
    ///
    /// Analogous to withIso for Iso optics.
    let withAlgebraicLens
            (optic: H2<AlgebraicExchangeEncoding<'a,'b>,'a,'b>
                 -> H2<AlgebraicExchangeEncoding<'a,'b>,'s,'t>)
            (k: ('s -> 'a) -> ('s list -> 'b -> 't option) -> 'r) : 'r =
        let seed : H2<AlgebraicExchangeEncoding<'a,'b>,'a,'b> =
            AlgebraicExchange(id, fun _ b -> Some b) :> _
        let (AlgebraicExchange(view, cls)) = optic seed :?> AlgebraicExchange<'a,'b,'s,'t>
        k view cls

    /// Extract the focused value via a profunctor-encoded AlgebraicLens.
    ///
    ///   viewAlg (algebraicLensToP l) s  =  l.view s
    let viewAlg
            (optic: H2<AlgebraicExchangeEncoding<'a,'b>,'a,'b>
                 -> H2<AlgebraicExchangeEncoding<'a,'b>,'s,'t>)
            (s: 's) : 'a =
        withAlgebraicLens optic (fun view _ -> view s)

    /// Apply the classification algebra via a profunctor-encoded AlgebraicLens.
    ///
    ///   classifyP (algebraicLensToP l) training b  =  l.classify training b
    let classifyP
            (optic: H2<AlgebraicExchangeEncoding<'a,'b>,'a,'b>
                 -> H2<AlgebraicExchangeEncoding<'a,'b>,'s,'t>)
            (training: 's list) (b: 'b) : 't option =
        withAlgebraicLens optic (fun _ cls -> cls training b)
