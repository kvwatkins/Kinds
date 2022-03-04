# Kinds.Control.Lens

Concrete optic types with composition. Depends on `Kinds.Data`.

All types live in `namespace Kinds.Control.Lens`; open `Kinds.Control.Lens.Concrete` to bring
the concrete optic types and the `>.>` operator into scope.

---

## Optic Types

| Type                     | Focus             | Description                                  |
|--------------------------|-------------------|----------------------------------------------|
| `Adapter<'s,'t,'a,'b>`   | One (bijection)   | Isomorphism: lossless round-trip between types |
| `Lens<'s,'t,'a,'b>`      | One (total)       | Focus on a field that is always present      |
| `Prism<'s,'t,'a,'b>`     | Zero or one       | Focus on one branch of a sum type            |
| `Affine<'s,'t,'a,'b>`    | Zero or one       | Combined Lens + Prism                        |
| `Traversal<'s,'t,'a,'b>` | Zero or more      | Focus on multiple elements                   |
| `Grate<'s,'t,'a,'b>`     | Distributing      | Coexponential / closed structure             |
| `AlgebraicLens<'s,'t,'a,'b>` | One (total)   | Lawless lens that trades `set` for `residual`|

Type-preserving aliases (simple, non-polymorphic form):

```fsharp
type Iso<'s,'a>       = Adapter<'s,'s,'a,'a>
type Lens'<'s,'a>     = Lens<'s,'s,'a,'a>
type Prism'<'s,'a>    = Prism<'s,'s,'a,'a>
type Affine'<'s,'a>   = Affine<'s,'s,'a,'a>
type Traversal'<'s,'a>= Traversal<'s,'s,'a,'a>
type Grate'<'s,'a>    = Grate<'s,'s,'a,'a>
```

---

## Composition — `>.>`

All optic types compose left-to-right with `>.>`. The result type is automatically widened
to the most general optic that satisfies both constraints:

```fsharp
open Kinds.Control.Lens.Concrete

// Lens >.> Lens     → Lens
// Lens >.> Prism    → Affine
// Lens >.> Traversal → Traversal
// Prism >.> Lens    → Affine
// Prism >.> Prism   → Prism

let nameLens : Lens'<Person, string> =
    Lens(fun p -> p.Name, fun p n -> { p with Name = n })

let firstCharPrism : Prism'<string, char> =
    Prism(string, fun s -> if s.Length > 0 then Right s.[0] else Left s)

let firstCharOfName : Affine'<Person, char> = nameLens >.> firstCharPrism
```

---

## Traversal Utilities

```fsharp
Traversal.toList  t s    // extract all foci into a list
Traversal.modify  t f s  // apply f to every focus
Traversal.set     t b s  // replace every focus with b
Traversal.length  t s    // count the number of foci
```

---

## Grate Utilities

```fsharp
Grate.review  g b         // embed a single value into the structure
Grate.zipWith g f s1 s2   // zip two structures element-wise via f
```
