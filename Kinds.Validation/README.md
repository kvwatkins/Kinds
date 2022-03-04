# Kinds.Validation

Profunctor-based validation. Depends on `Kinds.Data`, `Kinds.Data.Profunctor`,
`Kinds.Control.Lens`, and `Kinds.Control.Lens.Profunctor`.

---

## Design

`ValidatorEncoding<'e>` is a profunctor in `('a,'b)` that wraps `'a -> Either<'e list, 'b>`.
It implements **both** `Cartesian` and `CoCartesian`:

- `Cartesian` (`first`, `second`) — lets `lensToP` compose validators over record fields.
- `CoCartesian` (`left_`, `right_`) — lets `prismToP` compose validators over sum-type variants.

This is the same profunctor machinery as `ReducerStatePEncoding` + `ReducerActionPEncoding`
in `Redux.Profunctor`, applied to the validation domain. The optics infrastructure is reused
without modification.

```fsharp
type ValidatorData<'e,'a,'b> =
    | ValidatorP of ('a -> Either<'e list, 'b>)
    interface H2<ValidatorEncoding<'e>, 'a, 'b>

type Validator<'e,'a,'b> = H2<ValidatorEncoding<'e>,'a,'b>
```

---

## `Validator` Module

```fsharp
Validator.run     : Validator<'e,'a,'b> -> 'a -> Either<'e list,'b>
Validator.succeed : ('a -> 'b) -> Validator<'e,'a,'b>
Validator.fail    : 'e -> Validator<'e,'a,'b>
Validator.andThen : Validator<'e,'a,'b> -> Validator<'e,'b,'c> -> Validator<'e,'a,'c>
Validator.combine : Validator<'e,'a,'b> -> Validator<'e,'a,'c> -> Validator<'e,'a,'b*'c>
```

- **`andThen`** — sequential composition; stops at the first failure.
- **`combine`** — parallel composition with **error accumulation**; both validators run
  independently on the same input and all errors are concatenated.

```fsharp
open Kinds.Validation
open Kinds.Data.Either

let v = Validator.combine
            (satisfies "must start with 'A'" (fun (s: string) -> s.StartsWith("A")))
            (satisfies "must end with 'Z'"   (fun (s: string) -> s.EndsWith("Z")))

Validator.run v "hello"    // Left ["must start with 'A'"; "must end with 'Z'"]
Validator.run v "AhelloZ"  // Right ("AhelloZ", "AhelloZ")
```

---

## Primitive Rules

Available as `[<AutoOpen>]` in `Kinds.Validation`:

```fsharp
nonEmpty   : Validator<string, string, string>
minLength  : int -> Validator<string, string, string>
maxLength  : int -> Validator<string, string, string>
inRange    : 'a -> 'a -> Validator<string, 'a, 'a>   // requires comparison
notNone    : Validator<string, 'a option, 'a>
satisfies  : string -> ('a -> bool) -> Validator<string, 'a, 'a>
```

```fsharp
Validator.run nonEmpty ""              // Left ["Value must not be empty"]
Validator.run (minLength 5) "abc"     // Left ["Must be at least 5 characters"]
Validator.run (inRange 1 10) 0        // Left ["Must be in range [1, 10]"]
Validator.run notNone (None: int opt) // Left ["Value is required"]
Validator.run notNone (Some 42)       // Right 42
```

---

## `zoom` and `zoomCase`

```fsharp
zoom     : (H2<ValidatorEncoding<'e>,'a,'a> -> H2<ValidatorEncoding<'e>,'b,'b>)
           -> Validator<'e,'a,'a> -> Validator<'e,'b,'b>

zoomCase : (H2<ValidatorEncoding<'e>,'a,'a> -> H2<ValidatorEncoding<'e>,'b,'b>)
           -> Validator<'e,'a,'a> -> Validator<'e,'b,'b>
```

Both are function application — the profunctor optic (from `lensToP` or `prismToP`) is
already specialised to `ValidatorEncoding<'e>` by the Cartesian / CoCartesian constraint,
so calling it on the inner validator produces the outer validator.

### `zoom` — validate record fields via `lensToP`

```fsharp
open Kinds.Control.Lens.Profunctor

type Person = { Name: string; Age: int }

let nameLens = Lens(fun p -> p.Name, fun p n -> { p with Name = n })
let ageLens  = Lens(fun p -> p.Age,  fun p a -> { p with Age  = a })

let nameV : Validator<string, Person, Person> = zoom (lensToP nameLens) nonEmpty
let ageV  : Validator<string, Person, Person> = zoom (lensToP ageLens)  (inRange 0 150)

// Accumulate errors across fields
let personValidator : Validator<string, Person, Person> =
    let combined = Validator.combine nameV ageV
    Validator.andThen combined (Validator.succeed fst)
```

### `zoomCase` — validate sum-type variants via `prismToP`

```fsharp
type Account = Premium of Person | Guest of string

let premiumPrism = Prism(Premium, fun acc ->
    match acc with
    | Premium p -> Right p
    | Guest _   -> Left acc)

let accountValidator : Validator<string, Account, Account> =
    zoomCase (prismToP premiumPrism) personValidator
// Premium accounts are fully validated.
// Guest accounts pass through as Right unchanged — the prism does not match.
```

---

## Demo

`Kinds.Validation.Demo` is a console application that walks through all of the above:

- Primitive rules (`nonEmpty`, `minLength`, `inRange`, `notNone`, `satisfies`)
- Error accumulation with `Validator.combine`
- Record-field zoom with `zoom (lensToP ...)`
- Sum-type zoom with `zoomCase (prismToP ...)`

Run it with:

```bash
dotnet run --project Kinds.Validation.Demo
```
