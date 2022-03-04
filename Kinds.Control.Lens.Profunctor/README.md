# Kinds.Control.Lens.Profunctor

Profunctor-encoded optics and operations. Depends on `Kinds.Data.Profunctor` and `Kinds.Control.Lens`.

---

## Core Idea

A profunctor optic is a rank-2 function parameterised by a profunctor constraint:

```fsharp
type POptic<'p,'s,'t,'a,'b> = H2<'p,'a,'b> -> H2<'p,'s,'t>
```

The required constraint on `'p` determines the optic class:

| Constraint              | Optic class |
|-------------------------|-------------|
| `ProFunctor`            | Iso         |
| `Cartesian`             | Lens        |
| `CoCartesian`           | Prism       |
| `Closed`                | Grate       |
| `Cartesian+CoCartesian` | Affine      |

All operations in this library work uniformly over any profunctor that satisfies the required constraint — including user-defined profunctors such as `ValidatorEncoding<'e>` from `Kinds.Validation`.

---

## Converting Concrete Optics to Profunctor Form

```fsharp
open Kinds.Control.Lens.Profunctor

let myLens  = lensToP    (Lens(fst, fun (_, y) x -> (x, y)))
let myPrism = prismToP   justPrism
let myIso   = adapterToP intStringIso
let myGrate = grateToP   pairGrate
let myAff   = affineToP  myAffine
```

The resulting `POptic` can be passed directly to `zoom`, `zoomCase`, or any other
operation that consumes a profunctor optic.

---

## Optic Operations

```fsharp
open Kinds.Control.Lens.Profunctor

view      optic s         // extract the focused value (Lens / Iso)
over      optic f s       // apply f to the focus (Lens / Iso / Traversal)
set       optic b s       // replace the focus with b
review    optic b         // build a structure from a value (Iso, Prism)
preview   prism s         // optionally extract (concrete Prism — uses Market)
previewP  optic s         // optionally extract (profunctor-polymorphic Affine)
matching  optic s         // extract the forward direction of an Iso
withIso   optic k         // access both directions of an Iso simultaneously
toListOf  optic s         // collect all foci into a list (Traversal)
zipWithGrate g f s1 s2    // zip two structures via a Grate
```

---

## Internal Profunctors

These are used internally to implement optic operations; they are not normally constructed directly.

| Type              | Implements       | Role                              |
|-------------------|------------------|-----------------------------------|
| `Forget<'r>`      | `Cartesian`      | Getter — extracts a value (`view`)|
| `Tagged`          | `CoCartesian`    | Constructor (`review`)            |
| `Exchange<'a,'b>` | `ProFunctor`     | Iso extractor (`matching`, `withIso`) |
| `Market<'a,'b>`   | `CoCartesian`    | Prism extractor (`preview`)       |
| `ForgetList<'r>`  | `Cartesian`+`Monoidal` | Fold (`toListOf`)           |
