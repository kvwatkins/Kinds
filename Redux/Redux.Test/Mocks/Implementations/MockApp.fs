module Redux.Test.Mocks.Implementations.MockApp

open Redux
open Redux.Test.Mocks.Definitions
open DomainA
open DomainB
open Fiber

let getNewStore () =
    let globalApplicationReducer: Reducer<GlobalState, GlobalAction, GlobalEnvironment> =
        let pullback = Fiber.pullback

        pullback domainAReducer domainAFiber
        >.> pullback domainBReducer domainBFiber

    let liveInitialGlobalState =
        { DomainAState = { AField1 = 0; AField2 = 0 }
          DomainBState = { BField1 = 0; BField2 = 0 } }

    let liveProvidedEnvironment =
        { DomainAEnvironment = domainAEnvironment
          DomainBEnvironment = domainBEnvironment }

    let store =
        createStore globalApplicationReducer liveInitialGlobalState liveProvidedEnvironment

    store
