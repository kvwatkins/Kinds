/// Annotated usage examples for the Kinds library.
///
/// Each section focuses on one abstraction. The examples are written as
/// XUnit [<Fact>] tests so they are both documentation and verified behaviour.
module Kind.Examples

open System
open Xunit
// TypeClass.ProFunctor re-declares Functor<'f>, BiFunctor<'p>, fmap, and bimap
// alongside dimap/lmap/rmap. Opening it last ensures the correct `Functor`
// definition is in scope — the same one that Maybe and Either inherit from.
open TypeClass.Functor
open TypeClass
open TypeClass.ProFunctor
open Encoding

// ─────────────────────────────────────────────────────────────────────────────
// 1. HIGHER-KINDED TYPE ENCODING
// ─────────────────────────────────────────────────────────────────────────────
//
// The foundation of this library is a pair of interfaces:
//
//   type H<'K, 'T>  = interface end          // one-argument type constructor
//   type H2<'K,'T1,'T2> = H<H<'K,'T1>,'T2>  // two-argument type constructor
//
// Any concrete type F<'a> can participate by implementing H<FTag, 'a>.
// The phantom type 'K (the "tag") uniquely identifies the type constructor F.
//
// You normally interact with the encoding through `wrap` / `unwrap`
// or through the type-class functions (`fmap`, `bimap`, `dimap`).

// ─────────────────────────────────────────────────────────────────────────────
// 2. FUNCTOR — maybe<'a>
// ─────────────────────────────────────────────────────────────────────────────
//
// A Functor lets you apply a function to the value(s) inside a container
// without changing the container's shape.
//
//   fmap : ('a -> 'b) -> F<'a> -> F<'b>
//
// `maybe<'a>` is the library's optional type, equivalent to F#'s `Option<'a>`.

module FunctorExamples =

    open Kinds.Data.Maybe

    [<Fact>]
    let ``fmap over Just applies the function`` () =
        let value : maybe<int> = Just 21
        // fmap lifts ((*) 2) into the maybe container
        let doubled = fmap ((*) 2) value
        Assert.Equal(Some 42, unwrap doubled)

    [<Fact>]
    let ``fmap over Nothing is a no-op`` () =
        let noValue : maybe<int> = Nothing
        let doubled = fmap ((*) 2) noValue
        Assert.Equal(None, unwrap doubled)

    [<Fact>]
    let ``fmap can change the element type`` () =
        let value  : maybe<int> = Just 42
        let result = fmap string value
        Assert.Equal(Some "42", unwrap result)

    [<Fact>]
    let ``fmap satisfies the identity law: fmap id x = x`` () =
        let value : maybe<int> = Just 7
        Assert.Equal(unwrap value, unwrap (fmap id value))

    [<Fact>]
    let ``fmap satisfies the composition law: fmap (f << g) = fmap f << fmap g`` () =
        let f = string
        let g = ((*) 2)
        let value : maybe<int> = Just 5
        let lhs = fmap (f << g) value       // one pass
        let rhs = fmap f (fmap g value)     // two passes
        Assert.Equal(unwrap lhs, unwrap rhs)

    [<Fact>]
    let ``pattern match on maybe`` () =
        let values = [ Just 1; Nothing; Just 3 ]
        let results =
            values
            |> List.choose (fun m ->
                match m with
                | Just n  -> Some (n * 10)
                | Nothing -> None)
        Assert.Equal<int list>([10; 30], results)


// ─────────────────────────────────────────────────────────────────────────────
// 3. BIFUNCTOR — either<'a,'b>
// ─────────────────────────────────────────────────────────────────────────────
//
// A BiFunctor has two type parameters and lets you map over each independently.
//
//   bimap : ('a -> 'b) -> ('c -> 'd) -> P<'a,'c> -> P<'b,'d>
//
// `either<'e,'a>` is the library's sum type.
// Convention: Left carries the "error" side, Right carries the "success" side.

module BiFunctorExamples =

    open Kinds.Data.Either

    [<Fact>]
    let ``bimap transforms the Right side`` () =
        let value : either<string, int> = Right 21
        // first fn = Left transform, second fn = Right transform
        let result = bimap id ((*) 2) value
        match result with
        | Right n -> Assert.Equal(42, n)
        | Left  _ -> failwith "expected Right"

    [<Fact>]
    let ``bimap transforms the Left side`` () =
        let value : either<string, int> = Left "error"
        let result = bimap (fun s -> s + "!") id value
        match result with
        | Left  s -> Assert.Equal("error!", s)
        | Right _ -> failwith "expected Left"

    [<Fact>]
    let ``bimap leaves the untouched side unchanged`` () =
        let value : either<string, int> = Right 5
        // Left function must not run when the value is Right
        let result = bimap (fun _ -> failwith "should not run") ((*) 10) value
        match result with
        | Right n -> Assert.Equal(50, n)
        | Left  _ -> failwith "expected Right"

    [<Fact>]
    let ``constructing and deconstructing either`` () =
        let ok  : either<string, int> = Right 42
        let err : either<string, int> = Left  "oops"

        let extract (e: either<string, int>) =
            match e with
            | Right n -> $"success: {n}"
            | Left  s -> $"failure: {s}"

        Assert.Equal("success: 42", extract ok)
        Assert.Equal("failure: oops", extract err)

    [<Fact>]
    let ``bimap identity law: bimap id id x = x`` () =
        let value : either<string, int> = Right 99
        let result = bimap id id value
        match (value, result) with
        | Right a, Right b -> Assert.Equal(a, b)
        | Left  a, Left  b -> Assert.Equal(a, b)
        | _ -> failwith "structure changed"


// ─────────────────────────────────────────────────────────────────────────────
// 4. PROFUNCTOR — Star<'f,'d,'c>
// ─────────────────────────────────────────────────────────────────────────────
//
// A ProFunctor P is:
//   - contravariant in its first parameter  (input side)
//   - covariant in its second parameter     (output side)
//
//   dimap : ('a -> 'b) -> ('c -> 'd) -> P<'b,'c> -> P<'a,'d>
//
// `Star<'f,'d,'c>` wraps a Kleisli arrow  'd -> F<'c>
// ('f is the underlying functor, 'd the input, 'c the output).
//
// dimap lets you pre-process the input and post-process the output,
// composing pure transformations around an effectful core.

module ProFunctorExamples =

    open Kinds.Data.Star
    open Kinds.Data.Maybe

    // A simple Star: wrap any int in Just
    let wrapInt : Star<Maybe, int, int> =
        Star (fun n -> Just n)

    [<Fact>]
    let ``run extracts the underlying function`` () =
        let result = unwrap (run wrapInt 42)
        Assert.Equal(Some 42, result)

    [<Fact>]
    let ``dimap pre-processes input and post-processes output`` () =
        // pre-process:  parse string -> int
        // post-process: multiply by 2, convert to string
        let star' =
            dimap
                (fun (s: string) -> Int32.Parse(s))
                (fun (n: int)    -> string (n * 2))
                wrapInt
        Assert.Equal(Some "42", unwrap (run star' "21"))

    [<Fact>]
    let ``lmap only touches the input`` () =
        // lmap : ('a -> 'b) -> P<'b,'c> -> P<'a,'c>
        let star' = lmap (fun (s: string) -> Int32.Parse(s)) wrapInt
        Assert.Equal(Some 7, unwrap (run star' "7"))

    [<Fact>]
    let ``rmap only touches the output`` () =
        // rmap : ('b -> 'c) -> P<'a,'b> -> P<'a,'c>
        let star' = rmap (fun (n: int) -> n * 3) wrapInt
        Assert.Equal(Some 12, unwrap (run star' 4))

    [<Fact>]
    let ``dimap composes with a Star that always returns Nothing`` () =
        let failStar : Star<Maybe, int, int> = Star (fun _ -> Nothing)
        let star' =
            dimap
                (fun (s: string) -> Int32.Parse(s))
                (fun (n: int)    -> string n)
                failStar
        Assert.Equal(None, unwrap (run star' "99"))

    [<Fact>]
    let ``dimap identity law: dimap id id x = x`` () =
        let original = unwrap (run wrapInt 5)
        let adapted  = unwrap (run (dimap id id wrapInt) 5)
        Assert.Equal(original, adapted)


// ─────────────────────────────────────────────────────────────────────────────
// 5. COMBINATORS
// ─────────────────────────────────────────────────────────────────────────────
//
// The library includes the standard SKI combinator calculus combinators,
// available both as descriptive names and single-letter abbreviations.
//
// These form the foundation for building function combinators without lambdas.

module CombinatorExamples =

    open Pervasive.Combinators
    open Pervasive.Combinators.Abbreviation

    [<Fact>]
    let ``K (constant) ignores its second argument`` () =
        Assert.Equal(5,       K 5 "ignored")
        Assert.Equal("hello", K "hello" 42)
        let alwaysZero = K 0
        Assert.Equal(0, alwaysZero 999)

    [<Fact>]
    let ``B (compose) is function composition`` () =
        // B f g x  =  f (g x)  — same as f << g
        let add1ThenDouble = B ((*) 2) ((+) 1)
        Assert.Equal(8, add1ThenDouble 3)   // double(3+1) = 8

    [<Fact>]
    let ``C (flip) swaps argument order`` () =
        // C f x y  =  f y x
        Assert.Equal(1, C (-) 4 5)          // (-) 5 4 = 1

    [<Fact>]
    let ``S (substitution) applies two functions to the same argument`` () =
        // S f g x  =  f x (g x)
        // (+) 3 ((*2) 3)  =  3 + 6  =  9
        Assert.Equal(9,  substitution (+) ((*) 2) 3)
        Assert.Equal(15, substitution (+) ((*) 2) 5)

    [<Fact>]
    let ``S' (liftA2) applies a binary function to two projections`` () =
        // S' f g h x  =  f (g x) (h x)
        let sumOfSquares = S' (+) (fun x -> x * x) (fun x -> x * x)
        Assert.Equal(18, sumOfSquares 3)    // 9 + 9
        Assert.Equal(50, sumOfSquares 5)    // 25 + 25

    [<Fact>]
    let ``T (thrush) applies a value to a function`` () =
        // T x f  =  f x  — argument-flipped application
        Assert.Equal("42", T 42 string)
        Assert.Equal(84,   T 42 ((*) 2))

    [<Fact>]
    let ``W (join) applies a function to its own argument`` () =
        // W f x  =  f x x
        Assert.Equal(6, join (+) 3)         // 3 + 3

    [<Fact>]
    let ``compose matches the built-in << operator`` () =
        let f = string
        let g = ((*) 2)
        Assert.Equal((f << g) 5, compose f g 5)


// ─────────────────────────────────────────────────────────────────────────────
// 6. COSTAR — dual of Star
// ─────────────────────────────────────────────────────────────────────────────
//
// Where Star<'f,'d,'c> wraps  'd -> H<'f,'c>  (produce a wrapped output),
// CoStar<'f,'d,'c> wraps     H<'f,'d> -> 'c   (consume a wrapped input).
//
// As a ProFunctor:
//   dimap (f: 'a -> 'b) (g: 'c -> 'd) (CoStar s) = CoStar (g << s << fmap f)
//                                                                  ^
//                                               pre-process with fmap because
//                                               the input is already inside the functor

module CoStarExamples =

    open Kinds.Data.CoStar
    open Kinds.Data.Maybe

    // A CoStar that extracts the int from a maybe<int>, defaulting to -1
    let extractOrMinus1 : CoStar<Maybe, int, int> =
        CoStar (fun mh ->
            match mh with
            | Just n  -> n
            | Nothing -> -1)

    [<Fact>]
    let ``run extracts the wrapped function`` () =
        let fn = run extractOrMinus1
        let result = fn (Just 42)
        Assert.Equal(42, result)

    [<Fact>]
    let ``CoStar default on Nothing`` () =
        let fn = run extractOrMinus1
        Assert.Equal(-1, fn Nothing)

    [<Fact>]
    let ``dimap pre-processes input via fmap and post-processes bare output`` () =
        // Pre-process: fmap (double the int inside the functor)
        // Post-process: convert the extracted int to a string
        let star' =
            dimap
                (fun n -> n * 2)   // lifted into maybe via fmap
                string             // applied to the bare output
                extractOrMinus1
        let fn = run star'
        // fmap (*2) (Just 5) = Just 10, then extract = 10, then string = "10"
        Assert.Equal("10", fn (Just 5))

    [<Fact>]
    let ``dimap identity law: dimap id id x = x`` () =
        let fn1 = run extractOrMinus1
        let fn2 = run (dimap id id extractOrMinus1)
        Assert.Equal(fn1 (Just 7), fn2 (Just 7))
        Assert.Equal(fn1 Nothing,  fn2 Nothing)


// ─────────────────────────────────────────────────────────────────────────────
// 7. CARTESIAN (STRONG) PROFUNCTOR — first and second
// ─────────────────────────────────────────────────────────────────────────────
//
// A Cartesian (Strong) profunctor adds two operations that thread an extra
// type through a product (pair):
//
//   first  : p a b -> p (a, c) (b, c)   — focus on the first  element of a pair
//   second : p a b -> p (c, a) (c, b)   — focus on the second element of a pair
//
// Star implements Cartesian:
//   first  (Star s) = Star (fun (a, c) -> fmap (fun b -> (b, c)) (s a))
//   second (Star s) = Star (fun (c, a) -> fmap (fun b -> (c, b)) (s a))
//
// This is the key ingredient used by `lensToP` to convert a concrete Lens
// into profunctor form.

module CartesianExamples =

    open Kinds.Data.Star
    open Kinds.Data.Maybe

    // A simple Star: parse an int string, wrapping result in maybe
    let parseInt : Star<Maybe, string, int> =
        Star (fun s ->
            match System.Int32.TryParse(s) with
            | true, n  -> Just n
            | false, _ -> Nothing)

    [<Fact>]
    let ``first focuses on the first element of a pair, passing the second through`` () =
        // first parseInt : Star<Maybe, (string * 'c), (int * 'c)>
        let pairStar : Star<Maybe, string * string, int * string> = first parseInt
        let fn = run pairStar
        // parses the first element, leaves the second unchanged
        match fn ("42", "label") with
        | Just (n, s) ->
            Assert.Equal(42, n)
            Assert.Equal("label", s)
        | Nothing -> failwith "expected Just"

    [<Fact>]
    let ``first propagates Nothing from a failed parse`` () =
        let pairStar : Star<Maybe, string * string, int * string> = first parseInt
        let fn = run pairStar
        match fn ("bad", "label") with
        | Nothing -> ()  // expected
        | Just _  -> failwith "expected Nothing"

    [<Fact>]
    let ``second focuses on the second element of a pair, passing the first through`` () =
        // second parseInt : Star<Maybe, (string * string), (string * int)>
        let pairStar : Star<Maybe, string * string, string * int> = second parseInt
        let fn = run pairStar
        match fn ("prefix", "7") with
        | Just (s, n) ->
            Assert.Equal("prefix", s)
            Assert.Equal(7, n)
        | Nothing -> failwith "expected Just"

    [<Fact>]
    let ``combining dimap and first chains pre-processing with pairing`` () =
        // lmap turns a string->string function into a pre-processor
        let upperFirst : Star<Maybe, string * int, string * int> =
            first (lmap (fun s -> s.ToUpper()) (Star (fun s -> Just s)))
        let fn = run upperFirst
        match fn ("hello", 42) with
        | Just (s, n) ->
            Assert.Equal("HELLO", s)
            Assert.Equal(42, n)
        | Nothing -> failwith "expected Just"


// ─────────────────────────────────────────────────────────────────────────────
// 8. CONCRETE TRAVERSAL
// ─────────────────────────────────────────────────────────────────────────────
//
// A Traversal focuses on zero or more elements inside a structure.
//
//   contents : 's -> 'a seq        — extract all focused values
//   fill     : 'b seq -> 's -> 't  — rebuild with new values
//
// Composition distributes the replacement sequence across nested structures,
// chunking it to match how many elements each sub-traversal focuses on.

module TraversalExamples =
    #nowarn "57"   // Traversal is marked [<Experimental>] — that's intentional

    open Kinds.Control.Lens
    open Kinds.Control.Lens.Concrete

    // A Traversal over all elements of a list
    let listTraversal<'a,'b> : Traversal<'a list, 'b list, 'a, 'b> =
        Traversal(
            (fun xs -> Seq.ofList xs),
            (fun bs _xs -> Seq.toList bs))

    // A Traversal over both fields of a pair (same type)
    let pairTraversal<'a> : Traversal<'a * 'a, 'a * 'a, 'a, 'a> =
        Traversal(
            (fun (a, b) -> seq { yield a; yield b }),
            (fun bs _ ->
                let lst = Seq.toList bs
                (lst.[0], lst.[1])))

    [<Fact>]
    let ``toList extracts all focused elements`` () =
        let xs = [1; 2; 3]
        Assert.Equal<int list>([1; 2; 3], Traversal.toList listTraversal xs)

    [<Fact>]
    let ``modify applies a function to every focused element`` () =
        let xs = [1; 2; 3]
        let result = Traversal.modify listTraversal ((*) 10) xs
        Assert.Equal<int list>([10; 20; 30], result)

    [<Fact>]
    let ``set replaces every focused element`` () =
        let xs = [1; 2; 3]
        let result = Traversal.set listTraversal 0 xs
        Assert.Equal<int list>([0; 0; 0], result)

    [<Fact>]
    let ``length counts focused elements`` () =
        Assert.Equal(3, Traversal.length listTraversal [1; 2; 3])
        Assert.Equal(0, Traversal.length listTraversal [])
        Assert.Equal(2, Traversal.length pairTraversal (10, 20))

    [<Fact>]
    let ``composed traversal distributes the replacement sequence`` () =
        // outer: list of lists; inner: elements of each inner list
        let nestedTraversal = listTraversal.Compose(listTraversal)
        let nested = [[1; 2]; [3; 4; 5]]
        // modify every element
        let result = Traversal.modify nestedTraversal ((*) 2) nested
        Assert.Equal<int list list>([[2; 4]; [6; 8; 10]], result)

    [<Fact>]
    let ``asTraversal on a Lens focuses on the single element`` () =
        let fstLens : Lens<int * string, int> =
            Lens(fst, fun (_, s) n -> (n, s))
        let t = fstLens.asTraversal
        Assert.Equal<int list>([42], Traversal.toList t (42, "hello"))
        let result = Traversal.modify t ((*) 2) (21, "world")
        Assert.Equal((42, "world"), result)


// ─────────────────────────────────────────────────────────────────────────────
// 9. PROFUNCTOR OPTICS
// ─────────────────────────────────────────────────────────────────────────────
//
// Profunctor optics represent Iso, Lens, and Prism as a single uniform type:
//
//   type POptic p s t a b = p a b -> p s t
//
// The profunctor constraint on 'p determines which optic class is in use:
//   Iso   — ProFunctor only
//   Lens  — Cartesian (Strong)
//   Prism — Cocartesian (Choice)
//
// Operations are recovered by supplying a specific profunctor:
//   view   — Forget r     (read the focus)
//   over   — Star Identity (modify the focus)
//   review — Tagged        (build from the focus)

module ProfunctorOpticsExamples =

    open Kinds.Control.Lens.Profunctor.Profunctor
    open Kinds.Control.Lens
    open Kinds.Control.Lens.Concrete

    // A concrete Lens onto the first element of a pair
    let fstLens : Lens<int * string, int * string, int, int> =
        Lens(fst, fun (_, s) n -> (n, s))

    // A concrete Adapter (Iso) between int and string
    let intStringIso : Adapter<int, int, string, string> =
        Adapter(string, int)

    // A concrete Prism matching Some / injecting Some
    let justPrism : Prism<int option, int option, int, int> =
        Prism(Some, fun s ->
            match s with
            | Some n -> Kinds.Data.Either.Right n
            | None   -> Kinds.Data.Either.Left None)

    [<Fact>]
    let ``view extracts the focused value through a lens`` () =
        let optic = lensToP fstLens
        Assert.Equal(42, view optic (42, "hello"))

    [<Fact>]
    let ``over modifies the focused value through a lens`` () =
        let optic = lensToP fstLens
        Assert.Equal((84, "hello"), over optic ((*) 2) (42, "hello"))

    [<Fact>]
    let ``set replaces the focused value through a lens`` () =
        let optic = lensToP fstLens
        Assert.Equal((99, "hello"), set optic 99 (42, "hello"))

    [<Fact>]
    let ``view through an adapter applies the forward direction`` () =
        let optic = adapterToP intStringIso
        Assert.Equal("42", view optic 42)

    [<Fact>]
    let ``review through an adapter applies the backward direction`` () =
        let optic = adapterToP intStringIso
        Assert.Equal(42, review optic "42")

    [<Fact>]
    let ``review through a prism embeds a value into the structure`` () =
        let optic = prismToP justPrism
        Assert.Equal(Some 7, review optic 7)

    [<Fact>]
    let ``preview returns Some when the prism matches`` () =
        Assert.Equal(Some 42, preview justPrism (Some 42))

    [<Fact>]
    let ``preview returns None when the prism does not match`` () =
        Assert.Equal(None, preview justPrism None)
