# Kinds.Prelude

Foundation layer for the Kinds HKT library. No dependencies beyond `System`.

---

## `H<'K,'T>` — Higher-Kinded Type Encoding

Simulates higher-kinded types in F# using phantom type parameters and subtyping:

```fsharp
type H<'K, 'T>                    = interface end   // one-argument type constructor
type H2<'K, 'T1, 'T2>             = H<H<'K,'T1>,'T2>
type H3<'K, 'T1, 'T2, 'T3>        = H<H<H<'K,'T1>,'T2>,'T3>
type H4<'K, 'T1, 'T2, 'T3, 'T4>   = H<H<H<H<'K,'T1>,'T2>,'T3>,'T4>
```

A concrete type `F<'a>` participates by implementing `H<FEncoding, 'a>`, where
`FEncoding` is a unique *tag type* that also carries the type class instance:

```fsharp
type MaybeEncoding() =
    inherit Functor<MaybeEncoding>()   // encoding IS the type class dictionary
    ...

type MaybeData<'a> =
    | Just of 'a
    | Nothing
    interface H<MaybeEncoding, 'a>    // concrete type declares itself an H<MaybeEncoding, 'a>
```

`H<MaybeEncoding, 'a>` is the moral equivalent of Haskell's `f a`.
`H2<'K,'a,'b>` encodes bifunctors and profunctors: `H<H<'K,'a>,'b>`.

---

## `getTypeSignature<'a>` — Type Class Dictionary

Retrieves the singleton instance of a tag type via reflection:

```fsharp
[<GeneralizableValue>]
let getTypeSignature<'a> : 'a = ...  // reflects the zero-argument constructor of 'a
```

Used by all generic call-sites to resolve the correct type class operation at compile time,
guided by a constraint like `'F :> Functor<'F>`:

```fsharp
let fmap<'a,'b,'f when 'f :> Functor<'f>> (f: 'a -> 'b) (h: H<'f,'a>) : H<'f,'b> =
    getTypeSignature<'f>.fmap f h
```

---

## `wrap` / `unwrap` — Inline Helpers

Inline helpers for tag types that expose `static member wrap/unwrap`:

```fsharp
let inline wrap   (o: 'o)         : H<^f, 'a> = (^f: (static member wrap   : 'o -> H<'f,'a>) o)
let inline unwrap (f: H<^f, 'a>)  : 'o        = (^f: (static member unwrap : H<'f,'a> -> 'o) f)
```

---

## `Hole` — IDE-Style Typed Holes

Idris-inspired placeholder for incomplete programs:

```fsharp
open Hole

let result : int = Hole?myHole  // raises NotImplementedException at runtime
                                 // IDE tooling shows the expected type
```

---

## Combinators

SKI combinator calculus plus extras, in `Pervasive.Combinators`:

| Name           | Abbrev | Definition                        |
|----------------|--------|-----------------------------------|
| `constant`     | `K`    | `fun x _ -> x`                   |
| `id`           | `I`    | `fun x -> x`                     |
| `thrush`       | `T`    | `fun x f -> f x`                 |
| `compose`      | `B`    | `fun f g x -> f (g x)`           |
| `flip`         | `C`    | `fun f x y -> f y x`             |
| `substitution` | `S`    | `fun f g x -> f x (g x)`         |
| `liftA2`       | `S'`   | `fun f g h x -> f (g x) (h x)`   |
| `join`         | `W`    | `fun f x -> f x x`               |

Single-letter aliases available via `open Pervasive.Combinators.Abbreviation`.

```fsharp
open Pervasive.Combinators
open Pervasive.Combinators.Abbreviation

K 5 "anything"          // 5      — constant
B ((*) 2) ((+) 1) 3    // 8      — compose: double(3+1)
C (-) 3 10              // 7      — flip: 10 - 3
join (+) 3              // 6      — W: f x x
```
