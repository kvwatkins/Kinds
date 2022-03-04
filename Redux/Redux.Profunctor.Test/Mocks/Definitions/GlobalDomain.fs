namespace Redux.Profunctor.Test.Mocks.Definitions

open DomainA
open DomainB

[<AutoOpen>]
module GlobalDomain =
    type GlobalState =
        { DomainAState: DomainAState
          DomainBState: DomainBState }

    type GlobalAction =
        | DomainAAction of DomainAAction
        | DomainBAction of DomainBAction

    type GlobalEnvironment =
        { DomainAEnvironment: DomainAEnvironment
          DomainBEnvironment: DomainBEnvironment }
