module Kinds.Validation.Test.ValidatorTests

open Xunit
open Kinds.Data.Either
open Kinds.Validation

// ── succeed / fail ────────────────────────────────────────────────────────────

[<Fact>]
let ``succeed | always returns Right`` () =
    let v = Validator.succeed (fun x -> x + 1)
    match Validator.run v 5 with
    | Right r -> Assert.Equal(6, r)
    | Left _  -> Assert.Fail("expected Right")

[<Fact>]
let ``fail | always returns Left with single error`` () =
    let v : Validator<string, int, int> = Validator.fail "bad"
    match Validator.run v 0 with
    | Left errs -> Assert.Equal<string list>(["bad"], errs)
    | Right _   -> Assert.Fail("expected Left")

// ── combine (error accumulation) ──────────────────────────────────────────────

[<Fact>]
let ``combine | both succeed returns pair`` () =
    let v1 = Validator.succeed (fun x -> x + 1)
    let v2 = Validator.succeed (fun x -> x * 2)
    let v  = Validator.combine v1 v2
    match Validator.run v 3 with
    | Right (a, b) -> Assert.Equal(4, a); Assert.Equal(6, b)
    | Left _       -> Assert.Fail("expected Right")

[<Fact>]
let ``combine | first fails returns first errors`` () =
    let v1 : Validator<string, int, int> = Validator.fail "e1"
    let v2 = Validator.succeed id
    let v  = Validator.combine v1 v2
    match Validator.run v 0 with
    | Left errs -> Assert.Equal<string list>(["e1"], errs)
    | Right _   -> Assert.Fail("expected Left")

[<Fact>]
let ``combine | second fails returns second errors`` () =
    let v1 = Validator.succeed id
    let v2 : Validator<string, int, int> = Validator.fail "e2"
    let v  = Validator.combine v1 v2
    match Validator.run v 0 with
    | Left errs -> Assert.Equal<string list>(["e2"], errs)
    | Right _   -> Assert.Fail("expected Left")

[<Fact>]
let ``combine | both fail accumulates all errors`` () =
    let v1 : Validator<string, int, int> = Validator.fail "e1"
    let v2 : Validator<string, int, int> = Validator.fail "e2"
    let v  = Validator.combine v1 v2
    match Validator.run v 0 with
    | Left errs -> Assert.Equal<string list>(["e1"; "e2"], errs)
    | Right _   -> Assert.Fail("expected Left")

// ── andThen ───────────────────────────────────────────────────────────────────

[<Fact>]
let ``andThen | chains two validators sequentially`` () =
    let nonNeg = satisfies "must be non-negative" (fun x -> x >= 0)
    let double = Validator.succeed (fun x -> x * 2)
    let v      = Validator.andThen nonNeg double
    match Validator.run v 5 with
    | Right r -> Assert.Equal(10, r)
    | Left _  -> Assert.Fail("expected Right")

[<Fact>]
let ``andThen | stops at first failure`` () =
    let fail1  : Validator<string, int, int> = Validator.fail "step1"
    let fail2  : Validator<string, int, int> = Validator.fail "step2"
    let v      = Validator.andThen fail1 fail2
    match Validator.run v 0 with
    | Left errs -> Assert.Equal<string list>(["step1"], errs)   // step2 never runs
    | Right _   -> Assert.Fail("expected Left")

// ── rules ─────────────────────────────────────────────────────────────────────

[<Fact>]
let ``nonEmpty | rejects empty string`` () =
    match Validator.run nonEmpty "" with
    | Left _  -> ()
    | Right _ -> Assert.Fail("expected Left")

[<Fact>]
let ``nonEmpty | accepts non-empty string`` () =
    match Validator.run nonEmpty "hi" with
    | Right r -> Assert.Equal("hi", r)
    | Left _  -> Assert.Fail("expected Right")

[<Fact>]
let ``minLength | rejects too-short string`` () =
    match Validator.run (minLength 5) "ab" with
    | Left _  -> ()
    | Right _ -> Assert.Fail("expected Left")

[<Fact>]
let ``maxLength | rejects too-long string`` () =
    match Validator.run (maxLength 3) "abcde" with
    | Left _  -> ()
    | Right _ -> Assert.Fail("expected Left")

[<Fact>]
let ``inRange | rejects out-of-range value`` () =
    match Validator.run (inRange 1 10) 0 with
    | Left _  -> ()
    | Right _ -> Assert.Fail("expected Left")

[<Fact>]
let ``inRange | accepts in-range value`` () =
    match Validator.run (inRange 1 10) 5 with
    | Right r -> Assert.Equal(5, r)
    | Left _  -> Assert.Fail("expected Right")

[<Fact>]
let ``notNone | rejects None`` () =
    match Validator.run notNone (None : int option) with
    | Left _  -> ()
    | Right _ -> Assert.Fail("expected Left")

[<Fact>]
let ``notNone | accepts Some value`` () =
    match Validator.run notNone (Some 42) with
    | Right r -> Assert.Equal(42, r)
    | Left _  -> Assert.Fail("expected Right")
