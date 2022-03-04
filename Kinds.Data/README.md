# Kinds.Data

Standard data types with type class instances. Depends on `Kinds.Base`.

---

## `either<'a,'b>` / `Either<'a,'b>`

Sum type: `Left of 'a | Right of 'b`. Implements `BiFunctor`.

```fsharp
open Kinds.Data.Either

let result : either<string, int> = Right 42
let mapped = bimap (fun e -> $"Error: {e}") ((*) 2) result   // Right 84
```

The `Either` module provides additional operations:

```fsharp
Either.fold (fun _ -> -1) id (Right 42)          // 42
Either.fold (fun _ -> -1) id (Left "err")         // -1
Either.right (Right 42)                           // Some 42
Either.right (Left "err")                         // None
Either.bind (fun n -> Right (n * 2)) (Right 21)  // Right 42
Either.map  ((*) 2) (Right 21)                   // Right 42
Either.isRight (Right 42)                         // true
Either.isLeft  (Left "err")                       // true
```

---

## `maybe<'a>` / `Maybe`

Optional type equivalent to `Option<'a>`. Implements `Functor` and `Monoid` (First semantics:
`empty = Nothing`; `combine` returns the first `Just`).

```fsharp
open Kinds.Data.Maybe

let value : maybe<int> = Just 21
let doubled = fmap ((*) 2) value   // Just 42

// Monoid (First)
combine (Just 1) (Just 2)   // Just 1 — first non-Nothing wins
combine Nothing  (Just 2)   // Just 2
mconcat [Nothing; Nothing; Just 42; Just 99]   // Just 42
```

---

## `Identity<'a>`

Transparent wrapper. Implements `Functor`. Used internally by optic `over` to thread
the updated value back through the structure.

```fsharp
open Kinds.Data.Identity

let wrapped = IdentityEncoding.wrap 42
let value   = runIdentity wrapped   // 42
```

---

## `Const<'r,'a>`

Ignores `'a`, holds a constant `'r`. Implements `Functor`. Used internally by optic `view`
to extract a focused value without modifying the structure.

```fsharp
open Kinds.Data.Const

let c : H<ConstEncoding<string>, int> = ConstEncoding.wrap "hello"
let r = runConst c   // "hello"
```
