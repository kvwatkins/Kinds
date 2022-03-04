module Kinds.Validation.Test.ZoomTests

open Xunit
open Encoding
open Kinds.Data.Either
open Kinds.Control.Lens.Concrete
open Kinds.Control.Lens.Profunctor
open Kinds.Validation

// ── Test domain ───────────────────────────────────────────────────────────────

type Address = { Street: string; Postcode: string }

let streetLens : Lens<Address, string> =
    Lens((fun a -> a.Street), (fun a v -> { a with Street = v }))

let postcodeLens : Lens<Address, string> =
    Lens((fun a -> a.Postcode), (fun a v -> { a with Postcode = v }))

type Location =
    | Known   of Address
    | Unknown

let knownPrism : Prism<Location, Address> =
    Prism(
        Known,
        fun loc ->
            match loc with
            | Known a -> Right a
            | Unknown -> Left Unknown)

// ── zoom (lensToP) ────────────────────────────────────────────────────────────

[<Fact>]
let ``zoom | lensToP focuses street validation on Address`` () =
    let streetV   = Validator.andThen nonEmpty (minLength 3)
    let addressV  = zoom (lensToP streetLens) streetV
    let valid     = { Street = "Baker Street"; Postcode = "NW1 6XE" }
    let invalid   = { Street = "";             Postcode = "NW1 6XE" }

    match Validator.run addressV valid with
    | Right a -> Assert.Equal(valid, a)
    | Left _  -> Assert.Fail("expected Right for valid address")

    match Validator.run addressV invalid with
    | Left _  -> ()
    | Right _ -> Assert.Fail("expected Left for empty street")

[<Fact>]
let ``zoom | lensToP leaves other fields unchanged on success`` () =
    let streetV  = nonEmpty
    let addressV = zoom (lensToP streetLens) streetV
    let addr     = { Street = "Main St"; Postcode = "AB1 2CD" }
    match Validator.run addressV addr with
    | Right a -> Assert.Equal("AB1 2CD", a.Postcode)
    | Left _  -> Assert.Fail("expected Right")

[<Fact>]
let ``zoom | combines two field validators with Validator.combine`` () =
    let streetV   = zoom (lensToP streetLens)   nonEmpty
    let postcodeV = zoom (lensToP postcodeLens) nonEmpty
    let addressV  = Validator.combine streetV postcodeV

    // Both fields empty → both errors accumulated
    let bad = { Street = ""; Postcode = "" }
    match Validator.run addressV bad with
    | Left errs -> Assert.Equal(2, errs.Length)
    | Right _   -> Assert.Fail("expected Left")

// ── zoomCase (prismToP) ───────────────────────────────────────────────────────

[<Fact>]
let ``zoomCase | prismToP validates Known variant`` () =
    let streetV  = nonEmpty
    let addressV = zoom (lensToP streetLens) streetV
    let locV     = zoomCase (prismToP knownPrism) addressV

    let validLoc   = Known { Street = "Oak Ave"; Postcode = "EX1 1AA" }
    let invalidLoc = Known { Street = "";         Postcode = "EX1 1AA" }

    match Validator.run locV validLoc with
    | Right l -> Assert.Equal(validLoc, l)
    | Left _  -> Assert.Fail("expected Right")

    match Validator.run locV invalidLoc with
    | Left _  -> ()
    | Right _ -> Assert.Fail("expected Left for empty street in Known")

[<Fact>]
let ``zoomCase | prismToP passes Unknown through as Right`` () =
    let streetV  = Validator.fail "always fails"
    let locV     = zoomCase (prismToP knownPrism) streetV

    match Validator.run locV Unknown with
    | Right l -> Assert.Equal(Unknown, l)   // passes through unchanged
    | Left _  -> Assert.Fail("expected Right for non-matching variant")
