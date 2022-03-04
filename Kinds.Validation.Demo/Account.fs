module Kinds.Validation.Demo.Account

open Kinds.Data.Either
open Kinds.Control.Lens.Concrete
open Person

// ── Domain type ───────────────────────────────────────────────────────────────

type Account =
    | Premium of Person
    | Guest   of string   // just a display name

// ── Prisms ────────────────────────────────────────────────────────────────────

let premiumPrism : Prism<Account, Person> =
    Prism(
        Premium,
        fun acc ->
            match acc with
            | Premium p -> Right p
            | Guest _   -> Left acc)

let guestPrism : Prism<Account, string> =
    Prism(
        Guest,
        fun acc ->
            match acc with
            | Guest name -> Right name
            | Premium _  -> Left acc)
