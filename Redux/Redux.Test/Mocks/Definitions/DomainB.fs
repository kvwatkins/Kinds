namespace Redux.Test.Mocks.Definitions

open Redux
open Prelude

[<AutoOpen>]
module DomainB =
    type DomainBAction =
        | IncBField1 of int
        | IncBField2 of int

    type DomainBState = { BField1: int; BField2: int }

    type DomainBEnvironment =
        { PrintBField1: int -> Effect<Never>
          PrintBField2: int -> Effect<Never> }