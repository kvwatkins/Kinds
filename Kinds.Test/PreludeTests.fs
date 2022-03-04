module Kind.PreludeTests

open Xunit
open Pervasive.Combinators
open Pervasive.Combinators.Abbreviation

// ─────────────────────────────────────────────────────────────────────────────
// COMBINATORS NOT YET COVERED IN Examples.fs
// (K, B, C, S, S', T, W, compose are already tested there)
// ─────────────────────────────────────────────────────────────────────────────

module CombinatorGapTests =

    [<Fact>]
    let ``Ki (kite) ignores its first argument and returns the second`` () =
        Assert.Equal(99,      Ki 42 99)
        Assert.Equal("hello", Ki "ignored" "hello")
        Assert.Equal(0,       (Ki 5) 0)

    [<Fact>]
    let ``A (apply) is standard function application`` () =
        Assert.Equal(7,    A ((+) 2) 5)
        Assert.Equal("42", A string  42)

    [<Fact>]
    let ``Q (revCompose) is left-to-right composition`` () =
        // Q f g x = g (f x)
        let toStr  : int -> string = string
        let toLen  : string -> int = fun s -> s.Length
        Assert.Equal(2, Q toStr toLen 42)    // "42" has length 2
        Assert.Equal(3, Q toStr toLen 100)   // "100" has length 3

    [<Fact>]
    let ``Q agrees with B (compose) with arguments reversed`` () =
        let f : int -> string = string
        let g : string -> int = fun s -> s.Length
        Assert.Equal(Q f g 42, B g f 42)

    [<Fact>]
    let ``S_ (chain) passes both g(x) and x to f`` () =
        // S_ f g x = f (g x) x
        let tag = S_ (fun s n -> sprintf "%s=%d" s n) string
        Assert.Equal("7=7",   tag 7)
        Assert.Equal("42=42", tag 42)

    [<Fact>]
    let ``S_ (chain) builds a pair of the transformed and original values`` () =
        let pair = S_ (fun s n -> (s, n)) string
        Assert.Equal(("99", 99), pair 99)

    [<Fact>]
    let ``F (finch) applies a binary function with swapped arguments`` () =
        // F x y f = f y x
        Assert.Equal(1,    F 1 2 (-))        // (-) 2 1 = 1
        Assert.Equal("ba", F "a" "b" (+))   // (+) "b" "a" = "ba"

    [<Fact>]
    let ``V (pairing) applies a binary function to two arguments in order`` () =
        // V x y f = f x y
        Assert.Equal(7,  V 3 4 (+))
        Assert.Equal(-1, V 3 4 (-))
        Assert.Equal("helloworld", V "hello" "world" (+))

    [<Fact>]
    let ``P (on) applies a preprocessing function to both arguments`` () =
        // P f g x y = f (g x) (g y)
        let sumLengths = P (+) (fun (s: string) -> s.Length)
        Assert.Equal(10, sumLengths "hello" "world")   // 5 + 5
        Assert.Equal(5,  sumLengths "hi" "abc")        // 2 + 3

    [<Fact>]
    let ``P (on) can implement compare-by`` () =
        let compareByLen = P compare (fun (s: string) -> s.Length)
        Assert.True(compareByLen "hi" "hello" < 0)   // 2 < 5
        Assert.True(compareByLen "hello" "hi" > 0)   // 5 > 2
        Assert.Equal(0, compareByLen "abc" "def")     // 3 = 3


// ─────────────────────────────────────────────────────────────────────────────
// FIX AND FORCE
// ─────────────────────────────────────────────────────────────────────────────

module FixForceTests =

    [<Fact>]
    let ``force evaluates a lazy value exactly once`` () =
        let mutable count = 0
        let lz = lazy (count <- count + 1; 42)
        Assert.Equal(0, count)
        Assert.Equal(42, force lz)
        Assert.Equal(1, count)
        // Lazy is memoised; second force does not re-evaluate
        Assert.Equal(42, force lz)
        Assert.Equal(1, count)

    [<Fact>]
    let ``force returns the value stored in the lazy`` () =
        Assert.Equal(99,      force (lazy 99))
        Assert.Equal("hello", force (lazy "hello"))

    [<Fact>]
    let ``fix with a constant function returns that constant`` () =
        // fix (fun _ -> x) = lazy x  (f never forces its argument)
        Assert.Equal(42,      force (fix (fun _ -> 42)))
        Assert.Equal("hello", force (fix (fun _ -> "hello")))

    [<Fact>]
    let ``fix returns a Lazy value that can be forced`` () =
        let v = fix (fun _ -> 1)
        Assert.Equal(1, v.Value)
