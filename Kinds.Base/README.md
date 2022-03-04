# Kinds.Base

Pure functional programming type class hierarchy. Depends on `Kinds.Prelude`.

---

## Type Class Hierarchy

```
Functor
  └── Applicative
        └── Monad
              └── MonadPlus ─────┐
                                 │
Alternative ───────────────────-─┘

BiFunctor

ProFunctor
  ├── Cartesian   (Strong)
  ├── CoCartesian (Choice)
  ├── Closed
  ├── Costrong
  ├── Monoidal
  └── Arrow       (requires Category + Cartesian)

Semigroup
  └── Monoid

Foldable
Traversable  (requires Applicative)

Comonad

Category
```

---

## Type Classes

| Class          | Key Operation(s)               | Description                                   |
|----------------|--------------------------------|-----------------------------------------------|
| `Functor`      | `fmap`                         | Map a pure function over a container          |
| `BiFunctor`    | `bimap`                        | Map over both type parameters independently   |
| `Applicative`  | `pure_`, `ap`                  | Lift values; apply wrapped functions          |
| `Monad`        | `bind`                         | Sequence effectful computations               |
| `Alternative`  | `altEmpty`, `altOr`            | Choice between two applicatives               |
| `MonadPlus`    | `guard`                        | Monad with failure and choice                 |
| `ProFunctor`   | `dimap`, `lmap`, `rmap`        | Contramap on input, covariant on output       |
| `Cartesian`    | `first`, `second`              | Thread products through a profunctor          |
| `CoCartesian`  | `left_`, `right_`              | Thread sums through a profunctor              |
| `Closed`       | `closed`                       | Thread exponentials through a profunctor      |
| `Costrong`     | `unfirst`, `unsecond`          | Remove threaded products                      |
| `Monoidal`     | `par`, `empty_`                | Parallel profunctor composition               |
| `Arrow`        | `arr`                          | Morphisms composable with `>>>`               |
| `Category`     | `id_`, `compose`               | Associatively composable morphisms            |
| `Semigroup`    | `combine`                      | Associative binary operation                  |
| `Monoid`       | `empty`, `combine`             | Semigroup with an identity element            |
| `Foldable`     | `foldMap`                      | Reduce a container to a value                 |
| `Traversable`  | `traverse`                     | Map with effects, preserving shape            |
| `Comonad`      | `extract`, `extend`            | Dual of Monad; context-aware computation      |

### Encoding conventions

- **Abstract classes** (`Functor`, `Applicative`, `Monad`): default implementations are derived
  from abstract members. Instances inherit the class and override abstract members only.
- **Interfaces** (`BiFunctor`, `ProFunctor`, `Cartesian`, `CoCartesian`, `Semigroup`, `Monoid`, …):
  no useful default exists, or a type must satisfy multiple independent constraints simultaneously.

Defaults automatically provided:
- `Applicative.fmap` derived from `pure_` + `ap`
- `Monad.ap`         derived from `bind` + `pure_`
- `ProFunctor.lmap`  derived from `dimap`
- `ProFunctor.rmap`  derived from `dimap`

---

## Usage

```fsharp
open TypeClass   // auto-opens all type class operations

let doubled = fmap ((*) 2) (Just 21)   // Just 42

// Type class instance is resolved via the encoding tag constraint:
// fmap requires 'F :> Functor<'F>, resolved to MaybeEncoding at compile time.
```
