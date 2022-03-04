/// Tests and examples for the seven type classes added from the Typeclassopedia:
/// Comonad, Category, Arrow, Foldable, Traversable, Alternative, MonadPlus.
///
/// Each section defines a minimal concrete instance inline, then exercises
/// the operations and checks the key laws.
module Kind.TypeclassTests

open Xunit
open Encoding
open TypeClass
open TypeClass.ProFunctor
open Kinds.Data.Fn

// ─────────────────────────────────────────────────────────────────────────────
// SHARED TEST INSTANCES
// ─────────────────────────────────────────────────────────────────────────────

// Pair (Env) comonad  ─  (value * environment)
//   extract  = fst
//   extend f = fun (a, e) -> (f (a, e), e)
type EnvData<'e,'a> = | EnvD of 'a * 'e interface H<EnvEncoding<'e>,'a>
and EnvEncoding<'e>() =
    inherit Comonad<EnvEncoding<'e>>()
    override __.extract (h: H<EnvEncoding<'e>,'a>) =
        let (EnvD(a,_)) = h :?> EnvData<'e,'a> in a
    override __.extend (f: H<EnvEncoding<'e>,'a> -> 'b) (h: H<EnvEncoding<'e>,'a>) =
        let (EnvD(_,e)) = h :?> EnvData<'e,'a>
        EnvD(f h, e) :> H<EnvEncoding<'e>,'b>

// List monoid  ─  used as the result type in Foldable tests
type ListMonData<'a> = | ListMon of 'a list interface H<ListMonEncoding,'a>
and ListMonEncoding() =
    interface Monoid<ListMonEncoding> with
        member _.empty<'a> () : H<ListMonEncoding,'a> = ListMon [] :> _
        member _.combine<'a> (l: H<ListMonEncoding,'a>) (r: H<ListMonEncoding,'a>) =
            let (ListMon xs) = l :?> ListMonData<'a>
            let (ListMon ys) = r :?> ListMonData<'a>
            ListMon(xs @ ys) :> _

// MaybeBox  ─  Option wrapper that is Functor + Foldable + Traversable
type MaybeBoxData<'a> = | MaybeBox of 'a option interface H<MaybeBoxEncoding,'a>
and MaybeBoxEncoding() =
    inherit Functor<MaybeBoxEncoding>()
    override __.fmap<'a,'b> (f: 'a -> 'b) (h: H<MaybeBoxEncoding,'a>) =
        let (MaybeBox opt) = h :?> MaybeBoxData<'a>
        MaybeBox(Option.map f opt) :> H<MaybeBoxEncoding,'b>

    interface Foldable<MaybeBoxEncoding> with
        member _.foldMap<'a,'m,'M when 'M :> Monoid<'M>>
                (f: 'a -> H<'M,'m>) (h: H<MaybeBoxEncoding,'a>) =
            let (MaybeBox opt) = h :?> MaybeBoxData<'a>
            match opt with
            | Some a -> f a
            | None   -> (getTypeSignature<'M> :> Monoid<'M>).empty<'m> ()

    interface Traversable<MaybeBoxEncoding> with
        member _.traverse<'a,'b,'f when 'f :> Applicative<'f>>
                (f: 'a -> H<'f,'b>) (h: H<MaybeBoxEncoding,'a>) =
            let (MaybeBox opt) = h :?> MaybeBoxData<'a>
            match opt with
            | Some a ->
                fmap<'b, H<MaybeBoxEncoding,'b>, 'f>
                    (fun b -> MaybeBox(Some b) :> H<MaybeBoxEncoding,'b>)
                    (f a)
            | None ->
                pure_<H<MaybeBoxEncoding,'b>, 'f>
                    (MaybeBox None :> H<MaybeBoxEncoding,'b>)

// IdApp  ─  Identity applicative: pure wrapper with no effects, used in Traversable tests
type IdAppData<'a> = | IdApp of 'a interface H<IdAppEncoding,'a>
and IdAppEncoding() =
    inherit Applicative<IdAppEncoding>()
    override __.pure_ (a: 'a) = IdApp a :> H<IdAppEncoding,'a>
    override __.ap (mf: H<IdAppEncoding,'a->'b>) (mx: H<IdAppEncoding,'a>) =
        let (IdApp f) = mf :?> IdAppData<'a->'b>
        let (IdApp a) = mx :?> IdAppData<'a>
        IdApp(f a) :> H<IdAppEncoding,'b>

// Opt  ─  Option monad + Alternative + MonadPlus, used in Alternative/MonadPlus tests
type OptData<'a> = | Opt of 'a option interface H<OptEncoding,'a>
and OptEncoding() =
    inherit Monad<OptEncoding>()
    override __.pure_ (a: 'a) = Opt(Some a) :> H<OptEncoding,'a>
    override __.bind (h: H<OptEncoding,'a>) (f: 'a -> H<OptEncoding,'b>) =
        let (Opt opt) = h :?> OptData<'a>
        match opt with
        | Some a -> f a
        | None   -> Opt None :> H<OptEncoding,'b>

    interface Alternative<OptEncoding> with
        member _.empty_<'a> () = Opt None :> H<OptEncoding,'a>
        member _.altOr<'a> (l: H<OptEncoding,'a>) (r: H<OptEncoding,'a>) =
            let (Opt lopt) = l :?> OptData<'a>
            match lopt with Some _ -> l | None -> r

    interface MonadPlus<OptEncoding>


// ─────────────────────────────────────────────────────────────────────────────
// 1. COMONAD  —  the dual of Monad
// ─────────────────────────────────────────────────────────────────────────────
//
// A Comonad lets you:
//   extract   :: w a -> a              — read the current focus
//   extend    :: (w a -> b) -> w a -> w b  — apply a context-sensitive function
//   duplicate :: w a -> w (w a)        — wrap each position in its own context
//
// We test with the pair (Env) comonad where the "environment" is the second
// component and the focused "value" is the first.

module ComonadExamples =

    let mkEnv (v: int) (e: string) : H<EnvEncoding<string>,int> =
        EnvD(v, e) :> _

    [<Fact>]
    let ``extract returns the focused value`` () =
        let pair = mkEnv 42 "context"
        Assert.Equal(42, extract<int, EnvEncoding<string>> pair)

    [<Fact>]
    let ``extract ignores the environment`` () =
        let p1 = mkEnv 1 "env-a"
        let p2 = mkEnv 1 "env-b"
        Assert.Equal(extract<int,EnvEncoding<string>> p1,
                     extract<int,EnvEncoding<string>> p2)

    [<Fact>]
    let ``extend applies a context-aware function, preserving the environment`` () =
        // double the value and append its string to the environment
        let f (h: H<EnvEncoding<string>,int>) =
            let (EnvD(v, e)) = h :?> EnvData<string,int>
            $"{e}/{v}"
        let pair    = mkEnv 7 "root"
        let result  = extend<int,string,EnvEncoding<string>> f pair
        let (EnvD(v, e)) = result :?> EnvData<string,string>
        Assert.Equal("root/7", v)    // the new value comes from f
        Assert.Equal("root",   e)    // the environment is unchanged

    [<Fact>]
    let ``fmap is derived from extend+extract and behaves like a standard functor`` () =
        let pair   = mkEnv 10 "env"
        let result = fmap<int,string,EnvEncoding<string>> string pair
        let (EnvD(v, e)) = result :?> EnvData<string,string>
        Assert.Equal("10",  v)
        Assert.Equal("env", e)

    [<Fact>]
    let ``duplicate wraps the structure in itself, keeping the same environment`` () =
        let pair    = mkEnv 42 "env"
        let doubled = duplicate<int, EnvEncoding<string>> pair
        // The outer layer holds the full original pair; inner extract gives the value
        let inner   = extract<H<EnvEncoding<string>,int>, EnvEncoding<string>> doubled
        let value   = extract<int, EnvEncoding<string>> inner
        Assert.Equal(42, value)

    [<Fact>]
    let ``comonad left identity: extend extract = id`` () =
        let pair = mkEnv 99 "env"
        let back = extend<int,int,EnvEncoding<string>> (extract<int,EnvEncoding<string>>) pair
        Assert.Equal(extract<int,EnvEncoding<string>> pair,
                     extract<int,EnvEncoding<string>> back)


// ─────────────────────────────────────────────────────────────────────────────
// 2. CATEGORY
// ─────────────────────────────────────────────────────────────────────────────
//
// A Category provides identity morphisms and associative composition.
//
//   catId     :: () -> a ~> a          (identity)
//   compose   :: b ~> c -> a ~> b -> a ~> c   (right-to-left)
//   Category.andThen  f g = compose g f        (left-to-right)
//
// We use FnEncoding — the function arrow — as the canonical Category instance.

module CategoryExamples =

    [<Fact>]
    let ``catId is the identity morphism`` () =
        let id_ = catId<int, FnEncoding> ()
        Assert.Equal(42, runFn id_ 42)
        Assert.Equal("hello", runFn (catId<string, FnEncoding> ()) "hello")

    [<Fact>]
    let ``compose chains functions right-to-left`` () =
        let toStr  : H2<FnEncoding,int,string> = arr<int,string,FnEncoding> string
        let strlen : H2<FnEncoding,string,int> = arr<string,int,FnEncoding> String.length
        // compose strlen toStr = String.length << string
        let composed = compose<int,string,int,FnEncoding> strlen toStr
        Assert.Equal(2, runFn composed 42)   // string 42 = "42", length = 2

    [<Fact>]
    let ``Category.andThen is left-to-right composition`` () =
        let toStr  : H2<FnEncoding,int,string> = arr<int,string,FnEncoding> string
        let strlen : H2<FnEncoding,string,int> = arr<string,int,FnEncoding> String.length
        let chained = Category.andThen<int,string,int,FnEncoding> toStr strlen
        Assert.Equal(2, runFn chained 42)

    [<Fact>]
    let ``compose satisfies left identity: compose catId f = f`` () =
        let f = arr<int,string,FnEncoding> string
        let withId = compose<int,int,string,FnEncoding> f (catId<int,FnEncoding> ())
        Assert.Equal(runFn f 7, runFn withId 7)

    [<Fact>]
    let ``compose satisfies right identity: compose f catId = f`` () =
        let f = arr<int,string,FnEncoding> string
        let withId = compose<int,string,string,FnEncoding> (catId<string,FnEncoding> ()) f
        Assert.Equal(runFn f 7, runFn withId 7)


// ─────────────────────────────────────────────────────────────────────────────
// 3. ARROW
// ─────────────────────────────────────────────────────────────────────────────
//
// An Arrow lifts pure functions into the category and provides combinators:
//
//   arr      :: (b -> c) -> a b c              (lift a pure function)
//   fanPar   :: a b c -> a b' c' -> a (b*b') (c*c')  (run two arrows in parallel)
//   fanOut   :: a b c -> a b d -> a b (c*d)    (fork one input into two arrows)
//   andThen  :: a b c -> a c d -> a b d        (sequence)
//
// All tests use FnEncoding.

module ArrowExamples =

    [<Fact>]
    let ``arr lifts a pure function`` () =
        let double = arr<int,int,FnEncoding> ((*) 2)
        Assert.Equal(84, runFn double 42)

    [<Fact>]
    let ``Arrow.andThen sequences two arrows left-to-right`` () =
        let toStr  = arr<int,string,FnEncoding> string
        let strlen = arr<string,int,FnEncoding> String.length
        let chain  = Arrow.andThen<int,string,int,FnEncoding> toStr strlen
        Assert.Equal(3, runFn chain 100)    // "100" has length 3

    [<Fact>]
    let ``Arrow.fanPar runs two arrows on independent components of a pair`` () =
        let double = arr<int,int,FnEncoding>      ((*) 2)
        let upper  = arr<string,string,FnEncoding> (fun s -> s.ToUpper())
        let both   = Arrow.fanPar<int,int,string,string,FnEncoding> double upper
        Assert.Equal((84, "HELLO"), runFn both (42, "hello"))

    [<Fact>]
    let ``Arrow.fanOut applies two arrows to the same input`` () =
        let toStr = arr<int,string,FnEncoding> string
        let isPos = arr<int,bool,FnEncoding>   (fun n -> n > 0)
        let fork  = Arrow.fanOut<int,string,bool,FnEncoding> toStr isPos
        Assert.Equal(("42", true),  runFn fork  42)
        Assert.Equal(("-1", false), runFn fork -1)

    [<Fact>]
    let ``arr id is the same as catId`` () =
        let arrId = arr<int,int,FnEncoding> id
        let catId_ = catId<int,FnEncoding> ()
        Assert.Equal(runFn arrId 7, runFn catId_ 7)


// ─────────────────────────────────────────────────────────────────────────────
// 4. FOLDABLE
// ─────────────────────────────────────────────────────────────────────────────
//
// A Foldable container can be reduced to a summary value via a Monoid:
//
//   foldMap :: Monoid m => (a -> m) -> t a -> m
//
// We test using MaybeBox (our Option wrapper) and ListMon (a list Monoid).

module FoldableExamples =

    let someBox (x: 'a) : H<MaybeBoxEncoding,'a> = MaybeBox(Some x) :> _
    let noneBox<'a> ()  : H<MaybeBoxEncoding,'a> = MaybeBox None     :> _

    let toList (h: H<ListMonEncoding,'a>) : 'a list =
        let (ListMon xs) = h :?> ListMonData<'a> in xs

    [<Fact>]
    let ``foldMap over Some accumulates the mapped value`` () =
        let result = foldMap<int,int,MaybeBoxEncoding,ListMonEncoding>
                        (fun n -> ListMon [n] :> _) (someBox 42)
        Assert.Equal<int list>([42], toList result)

    [<Fact>]
    let ``foldMap over None returns the monoid identity`` () =
        let result = foldMap<int,int,MaybeBoxEncoding,ListMonEncoding>
                        (fun n -> ListMon [n] :> _) (noneBox ())
        Assert.Equal<int list>([], toList result)

    [<Fact>]
    let ``foldMap can transform elements before accumulating`` () =
        let result = foldMap<int,string,MaybeBoxEncoding,ListMonEncoding>
                        (fun n -> ListMon [string n] :> _) (someBox 7)
        Assert.Equal<string list>(["7"], toList result)

    [<Fact>]
    let ``Foldable.fold reduces a container of monoidal values`` () =
        let monoidVal : H<ListMonEncoding,int> = ListMon [1;2;3] :> _
        let container : H<MaybeBoxEncoding, H<ListMonEncoding,int>> = MaybeBox(Some monoidVal) :> _
        let result = Foldable.fold<int, MaybeBoxEncoding, ListMonEncoding> container
        Assert.Equal<int list>([1;2;3], toList result)

    [<Fact>]
    let ``Foldable.fold over None is the monoid identity`` () =
        let container : H<MaybeBoxEncoding, H<ListMonEncoding,int>> = MaybeBox None :> _
        let result = Foldable.fold<int, MaybeBoxEncoding, ListMonEncoding> container
        Assert.Equal<int list>([], toList result)


// ─────────────────────────────────────────────────────────────────────────────
// 5. TRAVERSABLE
// ─────────────────────────────────────────────────────────────────────────────
//
// A Traversable container can be traversed in an applicative context:
//
//   traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
//   sequenceA :: Applicative f => t (f a) -> f (t a)
//
// We test MaybeBox (Traversable) using IdApp (Identity applicative).
// The Identity applicative has no effects, so traverse just maps.

module TraversableExamples =

    let unwrapApp (h: H<IdAppEncoding,'a>) : 'a =
        let (IdApp a) = h :?> IdAppData<'a> in a

    let unwrapBox (h: H<MaybeBoxEncoding,'a>) : 'a option =
        let (MaybeBox opt) = h :?> MaybeBoxData<'a> in opt

    [<Fact>]
    let ``traverse over Some applies the function and wraps the result`` () =
        let result =
            traverse<int,string,MaybeBoxEncoding,IdAppEncoding>
                (fun n -> IdApp(string n) :> _)
                (MaybeBox(Some 42) :> H<MaybeBoxEncoding,int>)
        let inner = unwrapApp result
        Assert.Equal(Some "42", unwrapBox inner)

    [<Fact>]
    let ``traverse over None does not call the function`` () =
        let mutable called = false
        let result =
            traverse<int,string,MaybeBoxEncoding,IdAppEncoding>
                (fun n -> called <- true; IdApp(string n) :> _)
                (MaybeBox None :> H<MaybeBoxEncoding,int>)
        Assert.False(called)
        let inner = unwrapApp result
        Assert.Equal(None, unwrapBox inner)

    [<Fact>]
    let ``traverse with identity function is equivalent to fmap`` () =
        let box_ = MaybeBox(Some 7) :> H<MaybeBoxEncoding,int>
        let viaTraverse =
            traverse<int,int,MaybeBoxEncoding,IdAppEncoding>
                (fun n -> IdApp n :> _) box_
        Assert.Equal(Some 7, unwrapBox (unwrapApp viaTraverse))

    [<Fact>]
    let ``Traversable.sequenceA swaps the two layers`` () =
        let inner : H<IdAppEncoding,int> = IdApp 99 :> _
        let boxed : H<MaybeBoxEncoding, H<IdAppEncoding,int>> = MaybeBox(Some inner) :> _
        let result = Traversable.sequenceA<int, MaybeBoxEncoding, IdAppEncoding> boxed
        // result : H<IdAppEncoding, H<MaybeBoxEncoding,int>>
        Assert.Equal(Some 99, unwrapBox (unwrapApp result))

    [<Fact>]
    let ``Traversable.sequenceA over None lifts the empty box into the applicative`` () =
        let boxed : H<MaybeBoxEncoding, H<IdAppEncoding,int>> = MaybeBox None :> _
        let result = Traversable.sequenceA<int, MaybeBoxEncoding, IdAppEncoding> boxed
        Assert.Equal(None, unwrapBox (unwrapApp result))


// ─────────────────────────────────────────────────────────────────────────────
// 6. ALTERNATIVE
// ─────────────────────────────────────────────────────────────────────────────
//
// Alternative adds choice to an Applicative:
//
//   altEmpty :: () -> f a         (neutral element — always fails)
//   altOr    :: f a -> f a -> f a (try first; fall back to second)
//   asum     :: [f a] -> f a      (first success in a list)
//
// We test with Opt (our Option monad that implements Alternative).

module AlternativeExamples =

    let unwrapOpt (h: H<OptEncoding,'a>) : 'a option =
        let (Opt opt) = h :?> OptData<'a> in opt

    let some (x: 'a) : H<OptEncoding,'a> = Opt(Some x) :> _
    let none<'a> ()  : H<OptEncoding,'a> = Opt None     :> _

    [<Fact>]
    let ``altEmpty is the neutral element`` () =
        Assert.Equal(None, unwrapOpt (altEmpty<int,OptEncoding> ()))

    [<Fact>]
    let ``altOr returns the first non-empty value`` () =
        Assert.Equal(Some 1, unwrapOpt (altOr<int,OptEncoding> (some 1) (some 2)))

    [<Fact>]
    let ``altOr falls back to the second when the first is empty`` () =
        Assert.Equal(Some 2, unwrapOpt (altOr<int,OptEncoding> (none ()) (some 2)))

    [<Fact>]
    let ``altOr returns None when both are empty`` () =
        Assert.Equal(None, unwrapOpt (altOr<int,OptEncoding> (none ()) (none ())))

    [<Fact>]
    let ``Alternative.asum returns the first success`` () =
        let xs : H<OptEncoding,int> list = [none (); some 1; some 2]
        Assert.Equal(Some 1, unwrapOpt (Alternative.asum<int,OptEncoding> xs))

    [<Fact>]
    let ``Alternative.asum over all empty returns empty`` () =
        let xs : H<OptEncoding,int> list = [none (); none ()]
        Assert.Equal(None, unwrapOpt (Alternative.asum<int,OptEncoding> xs))

    [<Fact>]
    let ``left identity: altOr empty_ x = x`` () =
        let x = some 42
        Assert.Equal(unwrapOpt x, unwrapOpt (altOr<int,OptEncoding> (none ()) x))

    [<Fact>]
    let ``right identity: altOr x empty_ = x`` () =
        let x = some 42
        Assert.Equal(unwrapOpt x, unwrapOpt (altOr<int,OptEncoding> x (none ())))


// ─────────────────────────────────────────────────────────────────────────────
// 7. MONADPLUS
// ─────────────────────────────────────────────────────────────────────────────
//
// MonadPlus is a Monad + Alternative satisfying the stricter MonadPlus laws.
// The primary utility function is:
//
//   guard :: bool -> f unit   — succeed with unit or fail entirely
//
// guard is the building block for monadic list comprehensions and logic programming.

module MonadPlusExamples =

    let unwrapOpt (h: H<OptEncoding,'a>) : 'a option =
        let (Opt opt) = h :?> OptData<'a> in opt

    [<Fact>]
    let ``guard true succeeds with unit`` () =
        let result = MonadPlus.guard<OptEncoding> true
        Assert.Equal(Some (), unwrapOpt result)

    [<Fact>]
    let ``guard false fails`` () =
        let result = MonadPlus.guard<OptEncoding> false
        Assert.Equal(None, unwrapOpt result)

    [<Fact>]
    let ``guard can filter in a monadic chain`` () =
        // bind (guard (n > 0)) (fun _ -> pure_ n)  ≡  Some n when n > 0, None otherwise
        let tryPositive (n: int) : H<OptEncoding,int> =
            let check  = MonadPlus.guard<OptEncoding> (n > 0)
            let keepN  = fun () -> pure_<int,OptEncoding> n
            Monad.Operators.(>>=) check keepN
        Assert.Equal(Some 5,  unwrapOpt (tryPositive  5))
        Assert.Equal(None,    unwrapOpt (tryPositive -1))
        Assert.Equal(None,    unwrapOpt (tryPositive  0))
