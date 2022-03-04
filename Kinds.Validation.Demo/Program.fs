module Kinds.Validation.Demo.Program

open Kinds.Data.Either
open Kinds.Validation
open Person
open Account
open Form

// ─── Formatting ───────────────────────────────────────────────────────────────

let banner (title: string) =
    printfn ""
    printfn "%s" (String.replicate 70 "═")
    printfn "  %s" title
    printfn "%s" (String.replicate 70 "═")

let section (title: string) =
    printfn ""
    printfn "  ┌─ %s" title

let note (msg: string)   = printfn "  │  %s" msg
let close ()             = printfn "  └─────────────────────────────────────────"

let showResult label r =
    match r with
    | Right _    -> note (sprintf "%s → OK" label)
    | Left errs  ->
        note (sprintf "%s → INVALID:" label)
        for e in errs do note (sprintf "     • %s" e)

// ─── Demo ─────────────────────────────────────────────────────────────────────

banner "Kinds.Validation — Profunctor-based validation"

printfn """
  SCENARIO
  ────────
  Validate a Person record with three fields (Name, Age, Email).
  Then compose that into an Account validator that handles two
  sum-type variants (Premium | Guest) using zoomCase.

  Errors accumulate across independent fields via Validator.combine.
"""

// ── 1. Basic validators ───────────────────────────────────────────────────────

banner "STEP 1 — Primitive validators"

section "nonEmpty / minLength / maxLength"
showResult "nonEmpty \"\"        " (Validator.run nonEmpty "")
showResult "nonEmpty \"hello\"   " (Validator.run nonEmpty "hello")
showResult "minLength 5 \"abc\"  " (Validator.run (minLength 5) "abc")
showResult "maxLength 3 \"hello\"" (Validator.run (maxLength 3) "hello")
close ()

section "inRange / notNone / satisfies"
showResult "inRange 1 10 → 0  " (Validator.run (inRange 1 10) 0)
showResult "inRange 1 10 → 5  " (Validator.run (inRange 1 10) 5)
showResult "notNone None      " (Validator.run notNone (None : int option))
showResult "notNone (Some 42) " (Validator.run notNone (Some 42))
close ()

// ── 2. Combine (error accumulation) ──────────────────────────────────────────

banner "STEP 2 — Error accumulation with Validator.combine"

printfn """
  Validator.combine v1 v2 runs both validators on the same input.
  Errors from both are collected, not short-circuited.
"""

section "combine two failing validators"
let bothFail =
    Validator.combine
        (satisfies "must start with 'A'" (fun (s: string) -> s.StartsWith("A")))
        (satisfies "must end with 'Z'"   (fun (s: string) -> s.EndsWith("Z")))
showResult "\"hello\" (neither condition)" (Validator.run bothFail "hello")
showResult "\"AhelloZ\" (both pass)      " (Validator.run bothFail "AhelloZ")
close ()

// ── 3. zoom: focus on record fields ──────────────────────────────────────────

banner "STEP 3 — zoom: validate record fields via lensToP"

printfn """
  zoom (lensToP nameLens) nameValidator
    : Validator<string, Person, Person>

  The lens provides get/set for the Name field.
  The validator runs only on Name; the rest of Person is threaded through.
"""

section "valid Person"
let valid = { Name = "Alice"; Age = 30; Email = "alice@example.com" }
showResult "personValidator valid  " (Validator.run personValidator valid)

section "invalid Person (multiple fields wrong)"
let bad = { Name = ""; Age = -1; Email = "not-an-email" }
showResult "personValidator invalid" (Validator.run personValidator bad)
close ()

// ── 4. zoomCase: validate a sum-type variant ──────────────────────────────────

banner "STEP 4 — zoomCase: validate sum-type variants via prismToP"

printfn """
  zoomCase (prismToP premiumPrism) personValidator
    : Validator<string, Account, Account>

  Premium accounts are validated fully.
  Guest accounts pass through unchanged — the prism does not match.
"""

section "Premium account — valid"
let premiumGood = Premium { Name = "Bob"; Age = 25; Email = "bob@example.com" }
showResult "accountValidator Premium valid  " (Validator.run accountValidator premiumGood)

section "Premium account — invalid name"
let premiumBad = Premium { Name = ""; Age = 200; Email = "not-email" }
showResult "accountValidator Premium invalid" (Validator.run accountValidator premiumBad)

section "Guest account — passes through unchanged"
let guest = Guest "anonymous"
showResult "accountValidator Guest          " (Validator.run accountValidator guest)
close ()

banner "SUMMARY"
printfn """
  Kinds.Validation uses the same profunctor machinery as Redux.Profunctor:

    zoom     ←→  Cartesian   (lensToP)    — record fields
    zoomCase ←→  CoCartesian (prismToP)   — sum-type variants
    combine  ←→  error accumulation       — parallel validation

  A validator is just a profunctor. The optics infrastructure is reused
  without modification.
"""
