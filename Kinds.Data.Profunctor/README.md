# Kinds.Data.Profunctor

Canonical profunctor instances. Depends on `Kinds.Base` and `Kinds.Data`.

---

## Profunctor Hierarchy

```
ProFunctor
  ├── Cartesian   (Strong)  — thread products  ×
  ├── CoCartesian (Choice)  — thread sums       +
  ├── Closed                — thread exponentials →
  ├── Costrong              — remove threaded products
  ├── Monoidal              — parallel composition
  └── Arrow       (requires Category + Cartesian)
```

The profunctor constraint determines which optic class a `POptic` encodes:

| Constraint              | Optic characterised |
|-------------------------|---------------------|
| `ProFunctor`            | Iso / Adapter       |
| `Cartesian`             | Lens                |
| `CoCartesian`           | Prism               |
| `Closed`                | Grate               |
| `Cartesian+CoCartesian` | Affine              |
| `Monoidal`              | Traversal           |

---

## `Star<'f,'d,'c>` — Kleisli Arrow `'d → H<'f,'c>`

Implements `ProFunctor` and `Cartesian`. The canonical interpretation for `view` and `over`.

```fsharp
open Kinds.Data.Star

let parseInt : Star<Maybe, string, int> =
    Star (fun s ->
        match System.Int32.TryParse s with
        | true, n  -> Just n
        | false, _ -> Nothing)

// dimap: pre-process input, post-process output
let parseAndDouble : Star<Maybe, string, string> =
    dimap id (fun n -> string (n * 2)) parseInt

// first: focus on fst of a pair, thread snd through
let pairStar : Star<Maybe, string * string, int * string> = first parseInt

match run pairStar ("42", "label") with
| Just (n, s) -> printfn "%d / %s" n s   // 42 / label
| Nothing     -> printfn "parse failed"
```

---

## `CoStar<'f,'d,'c>` — Dual Kleisli Arrow `H<'f,'d> → 'c`

Implements `ProFunctor` and `Closed`. The contravariant side is lifted via `fmap`.

```fsharp
open Kinds.Data.CoStar

let extractOrMinus1 : CoStar<Maybe, int, int> =
    CoStar (fun mh ->
        match mh with
        | Just n  -> n
        | Nothing -> -1)

// dimap(f)(g): fmap f on the wrapped input, g on the output
let adapted = dimap ((*) 2) string extractOrMinus1

run adapted (Just 5)   // "10"  — input doubled via fmap, then extracted, then stringified
run adapted Nothing    // "-1"
```

---

## `Fn<'a,'b>` — Function Arrow `'a → 'b`

Implements `ProFunctor`, `Cartesian`, `CoCartesian`, `Closed`, `Monoidal`, `Category`, `Arrow`.
The ground-truth instance: any optic that works for `Fn` works for any compatible profunctor.

```fsharp
open Kinds.Data.Fn

let f : Fn<int, string> = Fn string
let g : Fn<int, int>    = arr ((*) 2)

// dimap: pre- and post-compose pure functions
let h = dimap (fun s -> int s) (fun n -> string (n * 2)) f
```
