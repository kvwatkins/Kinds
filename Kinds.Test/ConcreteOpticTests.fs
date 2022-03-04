module Kind.ConcreteOpticTests

open Xunit
open Kinds.Data.Either
open Kinds.Control.Lens
open Kinds.Control.Lens.Concrete

#nowarn "57"   // Traversal is marked [<Experimental>] — intentional

// ─────────────────────────────────────────────────────────────────────────────
// SHARED SAMPLE DATA
// ─────────────────────────────────────────────────────────────────────────────

/// Iso between int and string
let intStrIso : Adapter<int, int, string, string> = Adapter(string, int)

/// Lens onto the first component of a pair
let fstLens : Lens<int*string, int*string, int, int> =
    Lens(fst, fun (_, s) n -> (n, s))

/// Prism matching the Some case of int option
let justPrism : Prism<int option, int option, int, int> =
    Prism(Some, fun s ->
        match s with
        | Some n -> Right n
        | None   -> Left None)

/// Grate over a pair (f focuses on both components independently)
let pairGrate : Grate<int*int, int*int, int, int> =
    Grate(fun f -> (f fst, f snd))

/// Affine traversal: focuses on the Some value of an int option
let optAffine : Affine<int option, int option, int, int> =
    Affine(fun s ->
        match s with
        | Some n -> (Right n, Some)
        | None   -> (Left None, fun _ -> None))


// ─────────────────────────────────────────────────────────────────────────────
// 1. Adapter
// ─────────────────────────────────────────────────────────────────────────────
module AdapterTests =

    [<Fact>]
    let ``to_ applies the forward function`` () =
        Assert.Equal("42",  intStrIso.to_ 42)
        Assert.Equal("0",   intStrIso.to_ 0)
        Assert.Equal("-1",  intStrIso.to_ -1)

    [<Fact>]
    let ``from applies the backward function`` () =
        Assert.Equal(42,  intStrIso.from "42")
        Assert.Equal(0,   intStrIso.from "0")
        Assert.Equal(-99, intStrIso.from "-99")

    [<Fact>]
    let ``Adapter round-trips: from (to_ n) = n`` () =
        for n in [0; 1; -1; 42; 100] do
            Assert.Equal(n, intStrIso.from (intStrIso.to_ n))

    [<Fact>]
    let ``asLens get uses to_`` () =
        let l = intStrIso.asLens
        Assert.Equal("42", l.get 42)
        Assert.Equal("0",  l.get 0)

    [<Fact>]
    let ``asLens set ignores source and applies from`` () =
        let l = intStrIso.asLens
        // set ignores the 'source' argument; uses from on the new value
        Assert.Equal(42,  l.set 0   "42")   // source 0 is irrelevant
        Assert.Equal(99,  l.set 100 "99")   // source 100 is irrelevant

    [<Fact>]
    let ``asPrism embed is the backward function`` () =
        let p = intStrIso.asPrism
        Assert.Equal(42, p.embed "42")
        Assert.Equal(0,  p.embed "0")

    [<Fact>]
    let ``asPrism match_ always returns Right for an iso`` () =
        let p = intStrIso.asPrism
        match p.match_ 42 with
        | Right s -> Assert.Equal("42", s)
        | Left _  -> Assert.True(false, "expected Right")

    [<Fact>]
    let ``Adapter Compose chains two isos`` () =
        // int <-> string <-> char list
        let strToChars : Adapter<string, string, char list, char list> =
            Adapter((fun s -> Seq.toList s), (fun cs -> System.String(Array.ofList cs)))
        let composed = intStrIso.Compose(strToChars)
        Assert.Equal<char list>(['4'; '2'], composed.to_ 42)
        Assert.Equal(42, composed.from ['4'; '2'])


// ─────────────────────────────────────────────────────────────────────────────
// 2. Grate utilities
// ─────────────────────────────────────────────────────────────────────────────
module GrateTests =

    [<Fact>]
    let ``Grate.review embeds a constant into both positions`` () =
        Assert.Equal((42, 42), Grate.review pairGrate 42)
        Assert.Equal((0, 0),   Grate.review pairGrate 0)

    [<Fact>]
    let ``Grate.zipWith applies a binary function across two structures`` () =
        Assert.Equal((4, 6),    Grate.zipWith pairGrate (+) (1, 2) (3, 4))
        Assert.Equal((-2, -2),  Grate.zipWith pairGrate (-) (1, 2) (3, 4))
        Assert.Equal((3, 8),    Grate.zipWith pairGrate (*) (3, 2) (1, 4))

    [<Fact>]
    let ``Grate.zipWith with min picks the smaller of each position`` () =
        Assert.Equal((1, 3), Grate.zipWith pairGrate min (1, 5) (3, 3))

    [<Fact>]
    let ``Grate composition chains two grates`` () =
        // A newtype-like grate over a single int:  Grate (fun f -> f id)
        let idGrate : Grate<int, int, int, int> = Grate(fun f -> f id)
        let composed = pairGrate.Compose(idGrate)
        // composed.review 42 = (42, 42)
        Assert.Equal((42, 42), Grate.review composed 42)
        // composed zipWith (+) (1,2) (3,4) = (1+3, 2+4) = (4, 6)
        Assert.Equal((4, 6), Grate.zipWith composed (+) (1, 2) (3, 4))


// ─────────────────────────────────────────────────────────────────────────────
// 3. Conversions
// ─────────────────────────────────────────────────────────────────────────────
module ConversionTests =

    [<Fact>]
    let ``Lens.asAffine affine returns Right when focus is always present`` () =
        let aff = fstLens.asAffine
        let (ta, bt) = aff.affine (42, "hello")
        match ta with
        | Right n -> Assert.Equal(42, n)
        | Left _  -> Assert.True(false, "expected Right")
        Assert.Equal((99, "hello"), bt 99)

    [<Fact>]
    let ``Prism.asAffine returns Right when prism matches`` () =
        let aff = justPrism.asAffine
        let (ta, bt) = aff.affine (Some 7)
        match ta with
        | Right n -> Assert.Equal(7, n)
        | Left _  -> Assert.True(false, "expected Right")
        Assert.Equal(Some 99, bt 99)

    [<Fact>]
    let ``Prism.asAffine returns Left when prism does not match`` () =
        let aff = justPrism.asAffine
        let (ta, _bt) = aff.affine None
        match ta with
        | Left None -> ()  // expected
        | _         -> Assert.True(false, "expected Left None")

    [<Fact>]
    let ``Prism.asTraversal focuses on zero elements when not matching`` () =
        let t = justPrism.asTraversal
        Assert.Equal<int list>([], Traversal.toList t None)

    [<Fact>]
    let ``Prism.asTraversal focuses on one element when matching`` () =
        let t = justPrism.asTraversal
        Assert.Equal<int list>([5], Traversal.toList t (Some 5))

    [<Fact>]
    let ``Prism.asTraversal can rebuild the structure`` () =
        let t = justPrism.asTraversal
        let result = Traversal.modify t ((*) 2) (Some 5)
        Assert.Equal(Some 10, result)

    [<Fact>]
    let ``Adapter.asAffine always returns Right`` () =
        let aff = intStrIso.asAffine
        let (ta, _) = aff.affine 42
        match ta with
        | Right s -> Assert.Equal("42", s)
        | Left _  -> Assert.True(false, "expected Right")

    [<Fact>]
    let ``Adapter.asTraversal focuses on the single element`` () =
        let t = intStrIso.asTraversal
        Assert.Equal<string list>(["42"], Traversal.toList t 42)


// ─────────────────────────────────────────────────────────────────────────────
// 4. Composition operators (>.>)
// ─────────────────────────────────────────────────────────────────────────────
module CompositionTests =

    [<Fact>]
    let ``Lens >.> Lens composes two lenses`` () =
        // Outer lens: focus on first pair of a nested pair
        let outerL : Lens<(int*int)*string, (int*int)*string, int*int, int*int> =
            Lens(fst, fun (_, s) p -> (p, s))
        let innerL : Lens<int*int, int*int, int, int> =
            Lens(fst, fun (_, b) n -> (n, b))
        let composed = outerL >.> innerL
        Assert.Equal(42, composed.get ((42, 7), "hello"))
        Assert.Equal(((99, 7), "hello"), composed.set ((0, 7), "hello") 99)

    [<Fact>]
    let ``Adapter >.> Adapter composes two isos`` () =
        let strToChars : Adapter<string, string, char list, char list> =
            Adapter((fun s -> Seq.toList s), (fun cs -> System.String(Array.ofList cs)))
        let composed = intStrIso >.> strToChars
        Assert.Equal<char list>(['4'; '2'], composed.to_ 42)
        Assert.Equal(42, composed.from ['4'; '2'])

    [<Fact>]
    let ``Prism >.> Prism composes two prisms`` () =
        // Outer: Some of option option; inner: Some of option
        let outerP : Prism<int option option, int option option, int option, int option> =
            Prism(Some, fun s -> match s with Some a -> Right a | None -> Left None)
        let innerP : Prism<int option, int option, int, int> =
            Prism(Some, fun s -> match s with Some n -> Right n | None -> Left None)
        let composed = outerP >.> innerP
        Assert.Equal<int option option>(Some (Some 42), composed.embed 42)
        match composed.match_ (Some (Some 7)) with
        | Right n -> Assert.Equal(7, n)
        | Left  _ -> Assert.True(false, "expected Right")
        match composed.match_ (Some None) with
        | Left _ -> ()   // not matching: Left something
        | Right _ -> Assert.True(false, "expected Left")

    [<Fact>]
    let ``Adapter >.> Lens produces a lens focused through the iso`` () =
        // Adapter<int,int,string,string> >.> Lens<string,...,int,int>
        let lenLens : Lens<string, string, int, int> =
            Lens((fun (s:string) -> s.Length), fun _ n -> String.replicate n "x")
        let composed = intStrIso >.> lenLens
        Assert.Equal(2, composed.get 42)    // string 42 = "42", length = 2
        Assert.Equal(3, composed.get 100)   // string 100 = "100", length = 3

    [<Fact>]
    let ``Lens composed with Adapter produces a lens viewed through the iso`` () =
        // fstLens : Lens<int*string, int*string, int, int>
        // intStrIso : Adapter<int, int, string, string>
        // Result: Lens<int*string, int*string, string, string>
        // Note: use .Compose directly — the >.> overload for Lens*Adapter is constrained
        // by F# augmentation type-variable sharing and can't resolve in general position.
        let composed = fstLens.Compose(intStrIso.asLens)
        Assert.Equal("42",  composed.get (42, "hello"))
        Assert.Equal("0",   composed.get (0, "world"))


// ─────────────────────────────────────────────────────────────────────────────
// 5. Unsound.both
// ─────────────────────────────────────────────────────────────────────────────
module UnsoundBothTests =

    type Pt = { x: int; y: int }

    let xLens : Lens<Pt, int> = Lens((fun p -> p.x), (fun p n -> { p with x = n }))
    let yLens : Lens<Pt, int> = Lens((fun p -> p.y), (fun p n -> { p with y = n }))

    [<Fact>]
    let ``both get retrieves both fields as a pair`` () =
        let both = Unsound.both xLens yLens
        let pt = { x = 1; y = 2 }
        Assert.Equal((1, 2), both.get pt)

    [<Fact>]
    let ``both set updates both fields`` () =
        let both = Unsound.both xLens yLens
        let pt   = { x = 0; y = 0 }
        Assert.Equal({ x = 10; y = 20 }, both.set pt (10, 20))

    [<Fact>]
    let ``both round-trip: set then get`` () =
        let both = Unsound.both xLens yLens
        let pt   = { x = 5; y = 7 }
        let pt'  = both.set pt (42, 99)
        Assert.Equal((42, 99), both.get pt')
