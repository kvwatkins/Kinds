module Kind.ProfunctorOpticTests

open Xunit
open Encoding
open TypeClass
open TypeClass.ProFunctor
open Kinds.Data.Either
open Kinds.Control.Lens.Profunctor.Profunctor
open Kinds.Control.Lens.Concrete

// ─────────────────────────────────────────────────────────────────────────────
// SHARED SAMPLE OPTICS
// ─────────────────────────────────────────────────────────────────────────────

let intStrAdapter : Adapter<int, int, string, string> = Adapter(string, int)

let fstLens2 : Lens<int*string, int*string, int, int> =
    Lens(fst, fun (_, s) n -> (n, s))

let justPrism2 : Prism<int option, int option, int, int> =
    Prism(Some, fun s ->
        match s with
        | Some n -> Right n
        | None   -> Left None)

let pairGrate2 : Grate<int*int, int*int, int, int> =
    Grate(fun f -> (f fst, f snd))

let optAffine2 : Affine<int option, int option, int, int> =
    Affine(fun s ->
        match s with
        | Some n -> (Right n, Some)
        | None   -> (Left None, fun _ -> None))


// ─────────────────────────────────────────────────────────────────────────────
// 1. CoCartesian via TaggedEncoding
// ─────────────────────────────────────────────────────────────────────────────

module CoCartesianTaggedTests =

    [<Fact>]
    let ``left_ wraps the Tagged value in Left`` () =
        let tagged : H2<TaggedEncoding, int, int> = Tagged 42 :> _
        let leftTagged : H2<TaggedEncoding, Either<int,string>, Either<int,string>> =
            left_<int, int, string, TaggedEncoding> tagged
        let v : Either<int, string> = runTagged leftTagged
        match v with
        | Left n  -> Assert.Equal(42, n)
        | Right _ -> Assert.True(false, "expected Left")

    [<Fact>]
    let ``right_ wraps the Tagged value in Right`` () =
        let tagged : H2<TaggedEncoding, int, int> = Tagged 99 :> _
        let rightTagged : H2<TaggedEncoding, Either<string,int>, Either<string,int>> =
            right_<int, int, string, TaggedEncoding> tagged
        let v : Either<string, int> = runTagged rightTagged
        match v with
        | Right n -> Assert.Equal(99, n)
        | Left _  -> Assert.True(false, "expected Right")

    [<Fact>]
    let ``left_ and right_ are distinct branches`` () =
        let tagged : H2<TaggedEncoding, int, int> = Tagged 7 :> _
        let lv : Either<int, string> = runTagged (left_<int,int,string,TaggedEncoding> tagged)
        let rv : Either<string, int> = runTagged (right_<int,int,string,TaggedEncoding> tagged)
        Assert.True(match lv with Left _ -> true | _ -> false)
        Assert.True(match rv with Right _ -> true | _ -> false)


// ─────────────────────────────────────────────────────────────────────────────
// 2. grateToP
// ─────────────────────────────────────────────────────────────────────────────

module GrateToPTests =

    [<Fact>]
    let ``grateToP review embeds a constant into both positions`` () =
        let op = grateToP pairGrate2
        Assert.Equal((42, 42), review op 42)
        Assert.Equal((0, 0),   review op 0)

    [<Fact>]
    let ``grateToP review with negative value`` () =
        let op = grateToP pairGrate2
        Assert.Equal((-1, -1), review op -1)

    [<Fact>]
    let ``zipWithGrate applies a binary function across two pair structures`` () =
        Assert.Equal((4, 6),   zipWithGrate pairGrate2 (+) (1, 2) (3, 4))
        Assert.Equal((-2, -2), zipWithGrate pairGrate2 (-) (1, 2) (3, 4))

    [<Fact>]
    let ``zipWithGrate max picks the larger of each position`` () =
        Assert.Equal((3, 5), zipWithGrate pairGrate2 max (1, 5) (3, 3))


// ─────────────────────────────────────────────────────────────────────────────
// 3. affineTraversalToP
// ─────────────────────────────────────────────────────────────────────────────

module AffineTraversalToPTests =

    [<Fact>]
    let ``toListOf returns singleton for Some`` () =
        let op = affineTraversalToP optAffine2
        Assert.Equal<int list>([7],  toListOf op (Some 7))
        Assert.Equal<int list>([0],  toListOf op (Some 0))

    [<Fact>]
    let ``toListOf returns empty list for None`` () =
        let op = affineTraversalToP optAffine2
        Assert.Equal<int list>([], toListOf op None)

    [<Fact>]
    let ``toListOf agrees with direct match`` () =
        let op = affineTraversalToP optAffine2
        for n in [1; 0; -5; 42] do
            Assert.Equal<int list>([n], toListOf op (Some n))


// ─────────────────────────────────────────────────────────────────────────────
// 4. previewP
// ─────────────────────────────────────────────────────────────────────────────

module PreviewPTests =

    [<Fact>]
    let ``previewP returns Some when prism matches`` () =
        let op = prismToP justPrism2
        Assert.Equal(Some 42, previewP op (Some 42))
        Assert.Equal(Some 0,  previewP op (Some 0))

    [<Fact>]
    let ``previewP returns None when prism does not match`` () =
        let op = prismToP justPrism2
        Assert.Equal(None, previewP op None)

    [<Fact>]
    let ``previewP agrees with concrete preview`` () =
        let op = prismToP justPrism2
        for s in [Some 1; None; Some 99] do
            Assert.Equal(preview justPrism2 s, previewP op s)


// ─────────────────────────────────────────────────────────────────────────────
// 5. matching and withIso
// ─────────────────────────────────────────────────────────────────────────────

module IsoOpticTests =

    [<Fact>]
    let ``matching extracts the forward direction of an iso`` () =
        let op = adapterToP intStrAdapter
        Assert.Equal("42",  matching op 42)
        Assert.Equal("0",   matching op 0)
        Assert.Equal("-1",  matching op -1)

    [<Fact>]
    let ``withIso recovers both forward and backward functions`` () =
        let op = adapterToP intStrAdapter
        withIso op (fun to_ from ->
            Assert.Equal("7",  to_ 7)
            Assert.Equal(99,   from "99")
            Assert.Equal(0,    from "0"))

    [<Fact>]
    let ``withIso round-trips: from (to_ n) = n`` () =
        let op = adapterToP intStrAdapter
        withIso op (fun to_ from ->
            for n in [0; 1; -1; 42; 100] do
                Assert.Equal(n, from (to_ n)))


// ─────────────────────────────────────────────────────────────────────────────
// 6. toListOf
// ─────────────────────────────────────────────────────────────────────────────

module ToListOfTests =

    [<Fact>]
    let ``toListOf through a lens returns a singleton`` () =
        let op = lensToP fstLens2
        Assert.Equal<int list>([42], toListOf op (42, "hello"))
        Assert.Equal<int list>([0],  toListOf op (0, "x"))

    [<Fact>]
    let ``toListOf through a prism returns singleton when matching`` () =
        let op = prismToP justPrism2
        Assert.Equal<int list>([7],  toListOf op (Some 7))
        Assert.Equal<int list>([42], toListOf op (Some 42))

    [<Fact>]
    let ``toListOf through a prism returns empty when not matching`` () =
        let op = prismToP justPrism2
        Assert.Equal<int list>([], toListOf op None)

    [<Fact>]
    let ``toListOf through an adapter returns a singleton`` () =
        let op = adapterToP intStrAdapter
        Assert.Equal<string list>(["42"], toListOf op 42)
        Assert.Equal<string list>(["0"],  toListOf op 0)
