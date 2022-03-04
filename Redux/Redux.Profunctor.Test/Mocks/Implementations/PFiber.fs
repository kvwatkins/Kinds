module Redux.Profunctor.Test.Mocks.Implementations.PFiber

open Redux.Profunctor.Test.Mocks.Definitions
open Redux.Profunctor

let domainAFiber =
    { getState    = fun (s: GlobalState)      -> s.DomainAState
      setState    = fun s a                   -> { s with DomainAState = a }
      matchAction = fun b -> match b with DomainAAction a -> Some a | _ -> None
      embedAction = DomainAAction
      environment = fun (e: GlobalEnvironment) -> e.DomainAEnvironment }

let domainBFiber =
    { getState    = fun (s: GlobalState)      -> s.DomainBState
      setState    = fun s a                   -> { s with DomainBState = a }
      matchAction = fun b -> match b with DomainBAction a -> Some a | _ -> None
      embedAction = DomainBAction
      environment = fun (e: GlobalEnvironment) -> e.DomainBEnvironment }
