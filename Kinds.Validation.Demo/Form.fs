module Kinds.Validation.Demo.Form

open Kinds.Control.Lens.Profunctor
open Kinds.Validation
open Person
open Account

// ── Person field validators ───────────────────────────────────────────────────

let nameValidator    : Validator<string, string, string> =
    Validator.andThen nonEmpty (maxLength 100)

let ageValidator     : Validator<string, int, int> =
    inRange 0 150

let emailValidator   : Validator<string, string, string> =
    let combined = Validator.combine nonEmpty (satisfies "Must contain '@'" (fun s -> s.Contains('@')))
    Validator.andThen combined (Validator.succeed fst)

// ── Person validator (zoom each field) ───────────────────────────────────────

/// Validates all three fields of a Person, accumulating errors.
let personValidator : Validator<string, Person, Person> =
    let nameV  = zoom (lensToP nameLens)  nameValidator
    let ageV   = zoom (lensToP ageLens)   ageValidator
    let emailV = zoom (lensToP emailLens) emailValidator
    // combine produces Validator<_,Person,(Person*(Person*Person))>;
    // we map back to Person by keeping the first validated copy
    let combined = Validator.combine nameV (Validator.combine ageV emailV)
    Validator.andThen combined (Validator.succeed (fun (p, _) -> p))

// ── Account validator (zoomCase each variant) ─────────────────────────────────

/// Validates a Premium account's Person data; passes Guest through unchanged.
let accountValidator : Validator<string, Account, Account> =
    zoomCase (prismToP premiumPrism) personValidator
