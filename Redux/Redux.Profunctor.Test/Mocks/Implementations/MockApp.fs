module Redux.Profunctor.Test.Mocks.Implementations.MockApp

open Redux
open Redux.Profunctor
open Redux.Profunctor.Test.Mocks.Definitions
open DomainA
open DomainB
open PFiber

let getNewStore () =
    let globalApplicationReducer : Reducer<GlobalState, GlobalAction, GlobalEnvironment> =
        ppullback domainAReducer domainAFiber
        >.> ppullback domainBReducer domainBFiber

    let liveInitialGlobalState =
        { DomainAState = { AField1 = 0; AField2 = 0 }
          DomainBState = { BField1 = 0; BField2 = 0 } }

    let liveProvidedEnvironment =
        { DomainAEnvironment = domainAEnvironment
          DomainBEnvironment = domainBEnvironment }

    createStore globalApplicationReducer liveInitialGlobalState liveProvidedEnvironment
