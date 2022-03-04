module Kind.BaseTests

open Xunit
open Encoding
open TypeClass
open Kinds.Data.Maybe

// ─────────────────────────────────────────────────────────────────────────────
// TEST INSTANCES
// ─────────────────────────────────────────────────────────────────────────────

// Box: a trivial Identity applicative
type BoxData<'a> = Box of 'a interface H<BoxEncoding,'a>
and BoxEncoding() =
    inherit Applicative<BoxEncoding>()
    override __.pure_ (a: 'a) = Box a :> H<BoxEncoding,'a>
    override __.ap (mf: H<BoxEncoding,'a->'b>) (mx: H<BoxEncoding,'a>) =
        let (Box f) = mf :?> BoxData<'a->'b>
        let (Box a) = mx :?> BoxData<'a>
        Box (f a) :> H<BoxEncoding,'b>

// OptM: Option monad
type OptMData<'a> = OptM of 'a option interface H<OptMEncoding,'a>
and OptMEncoding() =
    inherit Monad<OptMEncoding>()
    override __.pure_ (a: 'a) = OptM (Some a) :> H<OptMEncoding,'a>
    override __.bind (m: H<OptMEncoding,'a>) (f: 'a -> H<OptMEncoding,'b>) =
        let (OptM opt) = m :?> OptMData<'a>
        match opt with
        | Some a -> f a
        | None   -> OptM None :> H<OptMEncoding,'b>

let boxUnwrap (h: H<BoxEncoding,'a>) : 'a =
    let (Box a) = h :?> BoxData<'a> in a

let optMUnwrap (h: H<OptMEncoding,'a>) : 'a option =
    let (OptM opt) = h :?> OptMData<'a> in opt


// ─────────────────────────────────────────────────────────────────────────────
// 1. SEMIGROUP and MONOID via Maybe (which implements Monoid)
// ─────────────────────────────────────────────────────────────────────────────
module SemigroupMonoidTests =

    [<Fact>]
    let ``combine (Just) (Nothing) = Just — first Just wins`` () =
        let l : maybe<int> = Just 1
        let r : maybe<int> = Nothing
        let result = combine<int, Maybe> l r
        Assert.Equal(Some 1, unwrap result)

    [<Fact>]
    let ``combine (Nothing) (Just) = Just — falls back to second`` () =
        let l : maybe<int> = Nothing
        let r : maybe<int> = Just 2
        let result = combine<int, Maybe> l r
        Assert.Equal(Some 2, unwrap result)

    [<Fact>]
    let ``combine (Just) (Just) = first Just`` () =
        let l : maybe<int> = Just 1
        let r : maybe<int> = Just 2
        let result = combine<int, Maybe> l r
        Assert.Equal(Some 1, unwrap result)

    [<Fact>]
    let ``combine (Nothing) (Nothing) = Nothing`` () =
        let l : maybe<int> = Nothing
        let r : maybe<int> = Nothing
        let result = combine<int, Maybe> l r
        Assert.Equal(None, unwrap result)

    [<Fact>]
    let ``empty gives Nothing`` () =
        let e = empty<int, Maybe> ()
        Assert.Equal(None, unwrap e)

    [<Fact>]
    let ``mconcat of empty list is Nothing`` () =
        let result = mconcat<int, Maybe> []
        Assert.Equal(None, unwrap result)

    [<Fact>]
    let ``mconcat picks the first Just in the list`` () =
        let xs : maybe<int> list = [Nothing; Just 3; Just 5; Nothing]
        let result = mconcat<int, Maybe> xs
        Assert.Equal(Some 3, unwrap result)

    [<Fact>]
    let ``mconcat of all Nothing is Nothing`` () =
        let xs : maybe<int> list = [Nothing; Nothing]
        let result = mconcat<int, Maybe> xs
        Assert.Equal(None, unwrap result)

    [<Fact>]
    let ``Semigroup associativity: combine (combine x y) z = combine x (combine y z)`` () =
        let x : maybe<int> = Just 1
        let y : maybe<int> = Nothing
        let z : maybe<int> = Just 3
        let lhs = combine<int,Maybe> (combine<int,Maybe> x y) z
        let rhs = combine<int,Maybe> x (combine<int,Maybe> y z)
        Assert.Equal(unwrap lhs, unwrap rhs)


// ─────────────────────────────────────────────────────────────────────────────
// 2. APPLICATIVE
// ─────────────────────────────────────────────────────────────────────────────
module ApplicativeTests =

    [<Fact>]
    let ``pure_ wraps a value`` () =
        let v = pure_<int, BoxEncoding> 42
        Assert.Equal(42, boxUnwrap v)

    [<Fact>]
    let ``ap applies a wrapped function to a wrapped value`` () =
        let mf = pure_<int->string, BoxEncoding> string
        let mx = pure_<int, BoxEncoding> 99
        let result = ap<int, string, BoxEncoding> mf mx
        Assert.Equal("99", boxUnwrap result)

    [<Fact>]
    let ``ap with addition`` () =
        let mf = pure_<int->int, BoxEncoding> ((+) 10)
        let mx = pure_<int, BoxEncoding> 5
        let result = ap<int, int, BoxEncoding> mf mx
        Assert.Equal(15, boxUnwrap result)

    [<Fact>]
    let ``fmap is derived from ap and pure_`` () =
        let mx   = pure_<int, BoxEncoding> 7
        let viaFmap = fmap<int, string, BoxEncoding> string mx
        let viaAp   = ap<int, string, BoxEncoding> (pure_<int->string, BoxEncoding> string) mx
        Assert.Equal(boxUnwrap viaFmap, boxUnwrap viaAp)

    [<Fact>]
    let ``Applicative identity law: ap (pure_ id) v = v`` () =
        let v = pure_<int, BoxEncoding> 42
        let result = ap<int, int, BoxEncoding> (pure_<int->int, BoxEncoding> id) v
        Assert.Equal(boxUnwrap v, boxUnwrap result)

    [<Fact>]
    let ``Applicative homomorphism: ap (pure_ f) (pure_ x) = pure_ (f x)`` () =
        let f = ((*) 3)
        let lhs = ap<int, int, BoxEncoding> (pure_<int->int, BoxEncoding> f) (pure_<int, BoxEncoding> 7)
        let rhs = pure_<int, BoxEncoding> (f 7)
        Assert.Equal(boxUnwrap rhs, boxUnwrap lhs)


// ─────────────────────────────────────────────────────────────────────────────
// 3. MONAD
// ─────────────────────────────────────────────────────────────────────────────
module MonadTests =

    open Monad.Operators

    [<Fact>]
    let ``bind sequences a computation`` () =
        let m = pure_<int, OptMEncoding> 5
        let result = bind<int, int, OptMEncoding> m (fun n -> pure_<int, OptMEncoding> (n * 2))
        Assert.Equal(Some 10, optMUnwrap result)

    [<Fact>]
    let ``bind short-circuits on None`` () =
        let m : H<OptMEncoding, int> = OptM None :> _
        let mutable called = false
        let result = bind<int, int, OptMEncoding> m (fun n ->
            called <- true
            pure_<int, OptMEncoding> (n * 2))
        Assert.False(called)
        Assert.Equal(None, optMUnwrap result)

    [<Fact>]
    let ``>>= operator sequences computations`` () =
        let m = pure_<int, OptMEncoding> 3
        let result = m >>= (fun n -> pure_<int, OptMEncoding> (n + 1))
        Assert.Equal(Some 4, optMUnwrap result)

    [<Fact>]
    let ``Monad left identity: bind (pure_ a) f = f a`` () =
        let f = fun n -> OptM (if n > 0 then Some (n * 2) else None) :> H<OptMEncoding,int>
        let lhs = bind<int, int, OptMEncoding> (pure_<int, OptMEncoding> 5) f
        let rhs = f 5
        Assert.Equal(optMUnwrap rhs, optMUnwrap lhs)

    [<Fact>]
    let ``Monad right identity: bind m pure_ = m`` () =
        let m = pure_<int, OptMEncoding> 42
        let result = bind<int, int, OptMEncoding> m (pure_<int, OptMEncoding>)
        Assert.Equal(optMUnwrap m, optMUnwrap result)

    [<Fact>]
    let ``chaining multiple binds`` () =
        let result =
            pure_<int, OptMEncoding> 2
            >>= (fun n -> pure_<int, OptMEncoding> (n * 3))
            >>= (fun n -> pure_<int, OptMEncoding> (n + 1))
        Assert.Equal(Some 7, optMUnwrap result)
