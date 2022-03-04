# Redux.Profunctor

Profunctor-based reducer composition. Depends on `Redux`, `Kinds.Data.Profunctor`,
and `Kinds.Control.Lens.Profunctor`.

Provides three zoom operations — `zoom`, `zoomAction`, and `magnify` — and a self-contained
`PFiber` / `ppullback` that performs the same job as `Fiber.pullback` from `Redux` without
depending on the concrete optics types in `Kinds.Control.Lens`.

---

## Design

`Reducer<'s,'a,'r>` is a function `'s -> 'a -> 'r -> ('s * Effect<'a>)`. Two profunctor
encodings treat it as a profunctor in one pair of type parameters at a time:

### `ReducerStatePEncoding<'act,'env>`

A `Cartesian` profunctor in the **state** coordinates `('s → 't)`.

- `dimap f g` pre-maps state-in via `f`, post-maps state-out via `g`.
- `first` threads an extra component `'c` through as a pair `(s,c) → (t,c)`.

This is exactly the machinery that `lensToP` requires — so `zoom (lensToP myLens)` works
on `ReducerStatePEncoding`.

### `ReducerActionPEncoding<'state,'env>`

A `CoCartesian` profunctor in the **action** coordinates `('a → 'b)`.

- `dimap f g` pre-maps action-in via `f`, post-maps action-out via `g`.
- `right_` threads a disjoint `'c` through as `Either<'c,'a> → Either<'c,'b>`.

This is exactly the machinery that `prismToP` requires — so `zoomAction (prismToP myPrism)`
works on `ReducerActionPEncoding`.

---

## Operations

### `zoom`

```fsharp
val zoom :
    optic   : (H2<ReducerStatePEncoding<'a,'r>,'s,'s> -> H2<ReducerStatePEncoding<'a,'r>,'t,'t>) ->
    reducer : Reducer<'s,'a,'r> ->
    Reducer<'t,'a,'r>
```

Widens a reducer's state type from `'s` to `'t` using a Lens-shaped profunctor optic.

```fsharp
open Redux.Profunctor
open Kinds.Control.Lens.Profunctor

let globalReducer = zoom (lensToP domainAStateLens) domainAReducer
// Reducer<DomainAState,'a,'r>  →  Reducer<GlobalState,'a,'r>
```

### `zoomAction`

```fsharp
val zoomAction :
    optic   : (H2<ReducerActionPEncoding<'s,'r>,'a,'a> -> H2<ReducerActionPEncoding<'s,'r>,'b,'b>) ->
    reducer : Reducer<'s,'a,'r> ->
    Reducer<'s,'b,'r>
```

Widens a reducer's action type from `'a` to `'b` using a Prism-shaped profunctor optic.
Actions that do not match the Prism cause the reducer to return the state unchanged with
`Effect.empty`.

```fsharp
let globalReducer = zoomAction (prismToP domainAActionPrism) domainAReducer
// Reducer<'s,DomainAAction,'r>  →  Reducer<'s,GlobalAction,'r>
```

### `magnify`

```fsharp
val magnify : proj: ('u -> 'r) -> reducer: Reducer<'s,'a,'r> -> Reducer<'s,'a,'u>
```

Contravariantly map the environment. No profunctor overhead needed — the environment
is consumed, not produced.

```fsharp
let globalReducer = magnify (fun g -> g.DomainAEnv) domainAReducer
// Reducer<'s,'a,DomainAEnv>  →  Reducer<'s,'a,GlobalEnv>
```

### Combining all three

The three operations compose to perform the same transformation as `Fiber.pullback`:

```fsharp
let lifted =
    domainAReducer
    |> magnify fiber.environment
    |> zoom    (lensToP domainAStateLens)
    |> zoomAction (prismToP domainAActionPrism)
```

---

## `PFiber` — Optics-Free Fiber

`PFiber` is a struct that packages the same three coordinate transformations as `Fiber`,
but using plain F# functions instead of concrete `Lens` / `Prism` types:

```fsharp
[<Struct>]
type PFiber<'t,'s,'b,'a,'u,'r> =
    { getState    : 't -> 's          // view
      setState    : 't -> 's -> 't   // set
      matchAction : 'b -> 'a option  // preview
      embedAction : 'a -> 'b         // review
      environment : 'u -> 'r }
```

`Redux.Profunctor` has no dependency on `Kinds.Control.Lens` — the profunctor optic for `zoom`
and `zoomAction` is constructed internally from `getState/setState` and `matchAction/embedAction`
using `first + dimap` and `right_ + dimap` on the encoding instances.

### `ppullback`

```fsharp
val ppullback :
    reducer : Reducer<'s,'a,'r> ->
    fiber   : PFiber<'t,'s,'b,'a,'u,'r> ->
    Reducer<'t,'b,'u>
```

Full equivalent of `Fiber.pullback`, using only `PFiber` fields:

```fsharp
let fiber =
    { getState    = fun g -> g.DomainA
      setState    = fun g s -> { g with DomainA = s }
      matchAction = fun b -> match b with DomainAAction a -> Some a | _ -> None
      embedAction = DomainAAction
      environment = fun g -> g.DomainAEnv }

let globalReducer = ppullback domainAReducer fiber
```

### `pscope`

```fsharp
val pscope :
    store : Store<'t,'b,'u> ->
    fiber : PFiber<'t,'s,'b,'a,'u,'r> ->
    Store<'s,'a,'r>
```

Creates a locally-typed scoped store from a global store, analogous to `Fiber.scope`.
