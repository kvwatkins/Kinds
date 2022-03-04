module Redux.Test.Mocks.Implementations.Fiber

open Redux.Test.Mocks.Definitions

open Prelude
open Redux
open Optics

let _domainAState =
    { get = fun s -> s.DomainAState
      set = fun s a -> { s with DomainAState = a } }

let _domainAAction =
    { build = DomainAAction
      match_ =
          fun s ->
              match s with
              | DomainAAction a -> Right a
              | _ -> Left s }

let _domainBState =
    { get = fun s -> s.DomainBState
      set = fun s a -> { s with DomainBState = a } }

let _domainBAction =
    { build = DomainBAction
      match_ =
          fun s ->
              match s with
              | DomainBAction a -> Right a
              | _ -> Left s }

let domainAFiber =
    { state = _domainAState
      action = _domainAAction
      environment = (fun (f: GlobalEnvironment) -> f.DomainAEnvironment) }

let domainBFiber =
    { state = _domainBState
      action = _domainBAction
      environment = (fun (f: GlobalEnvironment) -> f.DomainBEnvironment) }