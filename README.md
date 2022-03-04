# Kinds

An F# library for **higher-kinded type** polymorphism, **type class** encoding, and
composable abstractions — built purely from F#'s existing type system, without language extensions.

F# has no native support for abstracting over *type constructors* (`Option<_>`, `List<_>`,
`Result<_,_>`, …). Kinds works around this using a phantom-type interface encoding that lets you
write functions genuinely polymorphic over any compatible container or profunctor.

---

## The Core Idea

The entire library rests on a single interface:

```fsharp
type H<'K, 'T> = interface end
```

A type constructor `F<_>` joins this universe by:

1. Defining an *encoding tag* — a class that also carries the type class instance
2. Making the concrete type implement `H<Tag, 'a>`

```fsharp
type MaybeEncoding() =
    inherit Functor<MaybeEncoding>()
    override __.fmap f h = ...   // implement Functor

type MaybeData<'a> =
    | Just of 'a
    | Nothing
    interface H<MaybeEncoding, 'a>   // ← the "lift"
```

`H<MaybeEncoding, 'a>` is the moral equivalent of Haskell's `f a`.
Multi-parameter variants cover bifunctors and profunctors:

```fsharp
type H2<'K,'T1,'T2>             = H<H<'K,'T1>,'T2>
type H3<'K,'T1,'T2,'T3>         = H<H<H<'K,'T1>,'T2>,'T3>
type H4<'K,'T1,'T2,'T3,'T4>     = H<H<H<H<'K,'T1>,'T2>,'T3>,'T4>
```

---

## Project Map

```
Kinds/
├── Kinds.Prelude/            H<'K,'T>, H2-H4, getTypeSignature, wrap/unwrap, Hole, Combinators
├── Kinds.Base/               Type class hierarchy (Functor → Monad, ProFunctor → Closed, …)
├── Kinds.Data/               either, maybe, Identity, Const — data types with instances
├── Kinds.Data.Profunctor/    Star, CoStar, Fn — canonical profunctor instances
├── Kinds.Control.Lens/       Concrete optics: Adapter, Lens, Prism, Affine, Traversal, Grate
├── Kinds.Control.Lens.Profunctor/  POptic, view, over, set, review, preview, lensToP, prismToP
├── Kinds.Test/               222 unit tests and annotated usage examples
│
├── Redux/                    Elm-style state management (see Redux/README.md)
│   ├── Redux/                Core library: Effect, Reducer, Store, Fiber
│   ├── Redux.Test/           9 integration tests
│   ├── Redux.Demo/           Walkthrough using Fiber.pullback (Counter + Logger)
│   ├── Redux.Profunctor/     Profunctor-based pullback: zoom, zoomAction, magnify, PFiber
│   ├── Redux.Profunctor.Test/  19 unit tests
│   └── Redux.Profunctor.Demo/  Step-by-step profunctor composition walkthrough
│
├── Kinds.Validation/         Profunctor-based validation: Validator, Rules, zoom, zoomCase
├── Kinds.Validation.Test/    21 unit tests
└── Kinds.Validation.Demo/    Narrative demo — Person record, Account sum type, form validation
```

---

## Dependency Graph

```
Kinds.Prelude
  └── Kinds.Base
        ├── Kinds.Data
        │     └── Kinds.Data.Profunctor
        │           └── Kinds.Control.Lens
        │                 └── Kinds.Control.Lens.Profunctor
        │
        └── Redux
              ├── Redux.Profunctor  (+ Kinds.Data.Profunctor + Kinds.Control.Lens.Profunctor)
              └── Kinds.Validation  (+ Kinds.Data + Kinds.Control.Lens.Profunctor)
```

---

## Type Class Hierarchy

```
Functor
  └── Applicative ──> Monad ──> MonadPlus ──> Alternative

BiFunctor

ProFunctor
  ├── Cartesian   (Strong)  — thread products ×
  ├── CoCartesian (Choice)  — thread sums +
  ├── Closed                — thread exponentials →
  ├── Costrong, Monoidal, Arrow

Semigroup ──> Monoid

Foldable   Traversable   Comonad   Category
```

| Class          | Key Operation(s)          | Description                              |
|----------------|---------------------------|------------------------------------------|
| `Functor`      | `fmap`                    | Map over a container                     |
| `Applicative`  | `pure_`, `ap`             | Lift values; apply wrapped functions     |
| `Monad`        | `bind`                    | Sequence effectful computations          |
| `BiFunctor`    | `bimap`                   | Map over both type parameters            |
| `ProFunctor`   | `dimap`, `lmap`, `rmap`   | Contramap input, comap output            |
| `Cartesian`    | `first`, `second`         | Thread products through a profunctor     |
| `CoCartesian`  | `left_`, `right_`         | Thread sums through a profunctor         |
| `Semigroup`    | `combine`                 | Associative binary operation             |
| `Monoid`       | `empty`, `combine`        | Semigroup with an identity               |

---

## Quick Examples

### Functor — `fmap`

```fsharp
open Kinds.Data.Maybe

let doubled : maybe<int> = fmap ((*) 2) (Just 21)   // Just 42
let nothing : maybe<int> = fmap ((*) 2) Nothing      // Nothing
```

### BiFunctor — `bimap`

```fsharp
open Kinds.Data.Either

let result  : either<string, int> = Right 21
let mapped  = bimap (fun e -> $"Error: {e}") ((*) 2) result   // Right 42
```

### ProFunctor optics — `view` / `over`

```fsharp
open Kinds.Control.Lens.Profunctor

let fstLens = Lens(fst, fun (_, y) x -> (x, y))
let p = lensToP fstLens

view p ("hello", 42)           // "hello"
over p String.toUpper ("hi", 0)   // ("HI", 0)
set  p "world" ("hello", 0)    // ("world", 0)
```

### Validation — `zoom` + `zoomCase`

```fsharp
open Kinds.Validation
open Kinds.Control.Lens.Profunctor

let nameV = zoom (lensToP nameLens) nonEmpty
let ageV  = zoom (lensToP ageLens)  (inRange 0 150)

let personValidator =
    let combined = Validator.combine nameV ageV
    Validator.andThen combined (Validator.succeed fst)

Validator.run personValidator { Name = ""; Age = -1 }
// Left ["Value must not be empty"; "Must be in range [0, 150]"]
```

---

## Building & Testing

```bash
dotnet build Kinds.sln

dotnet test Kinds.sln
# Kinds.Test.dll              — 222 tests
# Redux.Test.dll              —   9 tests
# Redux.Profunctor.Test.dll   —  19 tests
# Kinds.Validation.Test.dll   —  21 tests

dotnet run --project Kinds.Validation.Demo
dotnet run --project Redux/Redux.Demo
dotnet run --project Redux/Redux.Profunctor.Demo
```

---

## Background & Further Reading

- **[Lightweight Higher-Kinded Polymorphism](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf)**
  (Yallop & White) — the academic basis for the interface-based phantom-type encoding
- **[higher (F#)](https://github.com/palladin/higher)** — a prior F# library using the same strategy
- **[Profunctor Optics](https://arxiv.org/abs/1703.10857)** (Pickering, Gibbons, Wu) — theoretical
  basis for `Kinds.Control.Lens.Profunctor`
- **[Haskell `Control.Lens`](https://hackage.haskell.org/package/lens)** — van Laarhoven and
  profunctor optics in practice
- **[Oleg Grenrus — Affine Traversals](http://oleg.fi/gists/posts/2017-03-20-affine-traversal.html)**
  — motivation for the `Affine` optic
- **[Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)**
  — conceptual background for functors, profunctors, and natural transformations

> **The key insight:** Because F# interfaces support subtyping, `H<'K, 'T>` acts as a
> phantom-typed slot that any `F<'T>` implementing the interface can fill. The phantom `'K`
> uniquely identifies the type constructor being abstracted over, giving us the effect of
> kind-polymorphism without any compiler extensions.
