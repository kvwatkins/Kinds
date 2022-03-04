module Kind.ProfunctorTests

open Xunit
open Encoding
open TypeClass
open TypeClass.ProFunctor
open Kinds.Data.Fn
open Kinds.Data.Maybe

// ─────────────────────────────────────────────────────────────────────────────
// 1. CoStrong on FnEncoding
// ─────────────────────────────────────────────────────────────────────────────

module CoStrongFnTests =

    [<Fact>]
    let ``unfirst on Fn projects out the first component`` () =
        // unfirst (fun (a, c) -> (a*2, c)) = fun a -> a*2
        let pab : H2<FnEncoding, int*int, int*int> = Fn (fun (a, c) -> (a * 2, c)) :> _
        let result = unfirst<int, int, int, FnEncoding> pab
        Assert.Equal(10, runFn result 5)
        Assert.Equal(0,  runFn result 0)

    [<Fact>]
    let ``unsecond on Fn projects out the second component`` () =
        // unsecond (fun (c, a) -> (c, a*3)) = fun a -> a*3
        let pab : H2<FnEncoding, int*int, int*int> = Fn (fun (c, a) -> (c, a * 3)) :> _
        let result = unsecond<int, int, int, FnEncoding> pab
        Assert.Equal(15, runFn result 5)
        Assert.Equal(6,  runFn result 2)

    [<Fact>]
    let ``unfirst preserves the pass-through component`` () =
        // For well-formed colenses the extra component threads through unchanged.
        // unfirst (fun (a, c) -> (a + c, c)) -- c passes through
        let pab : H2<FnEncoding, int*int, int*int> = Fn (fun (a, c) -> (a + c, c)) :> _
        let result = unfirst<int, int, int, FnEncoding> pab
        // defaultof<int> = 0, so result = fun a -> fst (a + 0, 0) = a
        Assert.Equal(5, runFn result 5)


// ─────────────────────────────────────────────────────────────────────────────
// 2. CoStrong on CoStarEncoding
// ─────────────────────────────────────────────────────────────────────────────

module CoStrongCoStarTests =

    [<Fact>]
    let ``unfirst on CoStar projects the first component after fmap pairing`` () =
        // CoStar that extracts the first element of a pair from a maybe
        let pab : H2<Kinds.Data.CoStar.CoStarEncoding<Maybe>, int*int, int*int> =
            Kinds.Data.CoStar.CoStar (fun m ->
                match m with
                | Just (a, c) -> (a * 2, c)
                | Nothing     -> (0, 0))
            :> _
        let result = unfirst<int, int, int, Kinds.Data.CoStar.CoStarEncoding<Maybe>> pab
        let fn = Kinds.Data.CoStar.run result
        Assert.Equal(10, fn (Just 5))
        Assert.Equal(0,  fn Nothing)

    [<Fact>]
    let ``unsecond on CoStar projects the second component after fmap pairing`` () =
        let pab : H2<Kinds.Data.CoStar.CoStarEncoding<Maybe>, int*int, int*int> =
            Kinds.Data.CoStar.CoStar (fun m ->
                match m with
                | Just (c, a) -> (c, a * 3)
                | Nothing     -> (0, 0))
            :> _
        let result = unsecond<int, int, int, Kinds.Data.CoStar.CoStarEncoding<Maybe>> pab
        let fn = Kinds.Data.CoStar.run result
        Assert.Equal(15, fn (Just 5))
        Assert.Equal(0,  fn Nothing)


// ─────────────────────────────────────────────────────────────────────────────
// 3. Closed on FnEncoding
// ─────────────────────────────────────────────────────────────────────────────

module ClosedFnTests =

    [<Fact>]
    let ``closed on Fn lifts through a function space`` () =
        // closed (Fn (+1)) : Fn (int -> int) (int -> int)
        // = Fn (fun (g: int -> int) -> (+1) << g)
        let add1 : H2<FnEncoding, int, int> = Fn ((+) 1) :> _
        let closedFn = closed<int, int, int, FnEncoding> add1
        let fn = runFn closedFn
        Assert.Equal(6, fn id 5)       // (+1) << id  applied to 5 = 6
        Assert.Equal(8, fn ((+) 2) 5)  // (+1) << (+2) applied to 5 = 8

    [<Fact>]
    let ``closed id is still a valid Closed value`` () =
        let identity : H2<FnEncoding, int, int> = Fn id :> _
        let closedId = closed<int, int, int, FnEncoding> identity
        let fn = runFn closedId
        Assert.Equal(5, fn id 5)
        Assert.Equal(7, fn ((+) 2) 5)


// ─────────────────────────────────────────────────────────────────────────────
// 4. Closed on CoStarEncoding
// ─────────────────────────────────────────────────────────────────────────────

module ClosedCoStarTests =

    [<Fact>]
    let ``closed on CoStar lifts through a function space`` () =
        // CoStar that adds 1 to the extracted int
        let kStar : H2<Kinds.Data.CoStar.CoStarEncoding<Maybe>, int, int> =
            Kinds.Data.CoStar.CoStar (fun m ->
                match m with Just n -> n + 1 | Nothing -> 0)
            :> _
        // closed kStar : CoStar<Maybe, int->int, int->int>
        // run (closed kStar) (Just (+2)) 3
        //   = kStar (fmap (fun f -> f 3) (Just (+2)))
        //   = kStar (Just 5) = 6
        let closedCS = closed<int, int, int, Kinds.Data.CoStar.CoStarEncoding<Maybe>> kStar
        let fn = Kinds.Data.CoStar.run closedCS
        Assert.Equal(6, fn (Just ((+) 2)) 3)
        Assert.Equal(0, fn Nothing 3)


// ─────────────────────────────────────────────────────────────────────────────
// 5. Monoidal on FnEncoding
// ─────────────────────────────────────────────────────────────────────────────

module MonoidalFnTests =

    [<Fact>]
    let ``mpar combines two Fn profunctors in parallel`` () =
        // mpar (Fn f) (Fn g) (a, c) = (f a, g c)
        let add1   : H2<FnEncoding, int, int> = Fn ((+) 1) :> _
        let double : H2<FnEncoding, int, int> = Fn ((*) 2) :> _
        let paired = mpar<int, int, int, int, FnEncoding> add1 double
        Assert.Equal((4, 8), runFn paired (3, 4))
        Assert.Equal((1, 0), runFn paired (0, 0))

    [<Fact>]
    let ``mempty_ is the identity on unit`` () =
        let empty = mempty_<FnEncoding> ()
        Assert.Equal((), runFn empty ())

    [<Fact>]
    let ``mpar with identity functions is equivalent to id on pairs`` () =
        let idFn : H2<FnEncoding, int, int> = Fn id :> _
        let paired = mpar<int, int, int, int, FnEncoding> idFn idFn
        Assert.Equal((3, 7), runFn paired (3, 7))
