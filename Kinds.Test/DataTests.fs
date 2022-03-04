module Kind.DataTests

open Xunit
open Encoding
open TypeClass
open Kinds.Data.Either
open Kinds.Data.Identity
open Kinds.Data.Const
open Kinds.Data.Maybe


// ─────────────────────────────────────────────────────────────────────────────
// 1. Either module functions
// ─────────────────────────────────────────────────────────────────────────────

module EitherModuleTests =

    [<Fact>]
    let ``Either.fold applies left function to Left`` () =
        let result = Either.fold (fun s -> s + "!") string (Left "err")
        Assert.Equal("err!", result)

    [<Fact>]
    let ``Either.fold applies right function to Right`` () =
        let result = Either.fold (fun s -> s + "!") string (Right 42)
        Assert.Equal("42", result)

    [<Fact>]
    let ``Either.right extracts Some from Right`` () =
        Assert.Equal(Some 42,  Either.right (Right 42  : either<string,int>))
        Assert.Equal(Some "x", Either.right (Right "x" : either<int,string>))

    [<Fact>]
    let ``Either.right returns None from Left`` () =
        Assert.Equal(None, Either.right (Left "err" : either<string,int>))

    [<Fact>]
    let ``Either.left extracts Some from Left`` () =
        Assert.Equal(Some "err", Either.left (Left "err" : either<string,int>))

    [<Fact>]
    let ``Either.left returns None from Right`` () =
        Assert.Equal(None, Either.left (Right 42 : either<string,int>))

    [<Fact>]
    let ``Either.mapLeft transforms the Left value`` () =
        let result = Either.mapLeft (fun s -> s + "!") (Left "err" : either<string,int>)
        match result with Left s -> Assert.Equal("err!", s) | _ -> Assert.True(false, "expected Left")

    [<Fact>]
    let ``Either.mapLeft leaves Right unchanged`` () =
        let result = Either.mapLeft (fun s -> s + "!") (Right 42 : either<string,int>)
        match result with Right n -> Assert.Equal(42, n) | _ -> Assert.True(false, "expected Right")

    [<Fact>]
    let ``Either.mapRight transforms the Right value`` () =
        let result = Either.mapRight ((*) 2) (Right 5 : either<string,int>)
        match result with Right n -> Assert.Equal(10, n) | _ -> Assert.True(false, "expected Right")

    [<Fact>]
    let ``Either.mapRight leaves Left unchanged`` () =
        let result = Either.mapRight ((*) 2) (Left "e" : either<string,int>)
        match result with Left s -> Assert.Equal("e", s) | _ -> Assert.True(false, "expected Left")

    [<Fact>]
    let ``Either.bind sequences on Right`` () =
        let result = Either.bind (fun n -> Right (n * 2)) (Right 5 : either<string,int>)
        match result with Right n -> Assert.Equal(10, n) | _ -> Assert.True(false, "expected Right")

    [<Fact>]
    let ``Either.bind with a Left-producing function`` () =
        let result = Either.bind (fun _ -> Left "overflow") (Right 5 : either<string,int>)
        match result with Left s -> Assert.Equal("overflow", s) | _ -> Assert.True(false, "expected Left")

    [<Fact>]
    let ``Either.bind short-circuits on Left`` () =
        let mutable called = false
        let result = Either.bind (fun n -> called <- true; Right (n * 2)) (Left "err" : either<string,int>)
        Assert.False(called)
        match result with Left s -> Assert.Equal("err", s) | _ -> Assert.True(false, "expected Left")


// ─────────────────────────────────────────────────────────────────────────────
// 2. Identity
// ─────────────────────────────────────────────────────────────────────────────

module IdentityTests =

    [<Fact>]
    let ``runIdentity extracts the wrapped value`` () =
        let id1 : H<IdentityEncoding, int> = Identity 42 :> _
        Assert.Equal(42, runIdentity id1)

    [<Fact>]
    let ``fmap transforms the wrapped value`` () =
        let id1 : H<IdentityEncoding, int> = Identity 7 :> _
        let mapped = fmap<int, string, IdentityEncoding> string id1
        Assert.Equal("7", runIdentity mapped)

    [<Fact>]
    let ``fmap identity law: fmap id x = x`` () =
        let id1 : H<IdentityEncoding, int> = Identity 99 :> _
        let mapped = fmap<int, int, IdentityEncoding> id id1
        Assert.Equal(runIdentity id1, runIdentity mapped)

    [<Fact>]
    let ``fmap composition law: fmap (f << g) = fmap f << fmap g`` () =
        let f = string
        let g = ((*) 2)
        let id1 : H<IdentityEncoding, int> = Identity 5 :> _
        let lhs = fmap<int, string, IdentityEncoding> (f << g) id1
        let rhs = fmap<int, string, IdentityEncoding> f (fmap<int, int, IdentityEncoding> g id1)
        Assert.Equal(runIdentity lhs, runIdentity rhs)

    [<Fact>]
    let ``wrap and unwrap are inverse`` () =
        let v = 123
        let wrapped = IdentityEncoding.wrap v
        Assert.Equal(v, IdentityEncoding.unwrap wrapped)


// ─────────────────────────────────────────────────────────────────────────────
// 3. Const
// ─────────────────────────────────────────────────────────────────────────────

module ConstTests =

    [<Fact>]
    let ``runConst extracts the stored value`` () =
        let c1 : H<ConstEncoding<string>, int> = Const "hello" :> _
        Assert.Equal("hello", runConst c1)

    [<Fact>]
    let ``fmap ignores the function and preserves the constant`` () =
        let c1 : H<ConstEncoding<string>, int> = Const "hello" :> _
        let mapped = fmap<int, bool, ConstEncoding<string>> (fun _ -> true) c1
        Assert.Equal("hello", runConst mapped)

    [<Fact>]
    let ``fmap type-changes the phantom but keeps the value`` () =
        let c1 : H<ConstEncoding<int>, string> = Const 42 :> _
        let mapped = fmap<string, float, ConstEncoding<int>> (fun _ -> 3.14) c1
        Assert.Equal(42, runConst mapped)

    [<Fact>]
    let ``wrap and unwrap are inverse`` () =
        let v = "test"
        let wrapped = ConstEncoding<string>.wrap v
        Assert.Equal(v, ConstEncoding<string>.unwrap wrapped)
