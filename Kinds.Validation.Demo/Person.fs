module Kinds.Validation.Demo.Person

open Kinds.Control.Lens.Concrete

// ── Domain type ───────────────────────────────────────────────────────────────

type Person = { Name: string; Age: int; Email: string }

// ── Lenses ────────────────────────────────────────────────────────────────────

let nameLens : Lens<Person, string> =
    Lens((fun p -> p.Name), (fun p v -> { p with Name = v }))

let ageLens : Lens<Person, int> =
    Lens((fun p -> p.Age), (fun p v -> { p with Age = v }))

let emailLens : Lens<Person, string> =
    Lens((fun p -> p.Email), (fun p v -> { p with Email = v }))
