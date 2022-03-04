namespace Redux.Profunctor.Test.Integration

open Redux
open Redux.Profunctor
open Redux.Profunctor.Test.Mocks.Implementations.MockApp
open Redux.Profunctor.Test.Mocks.Implementations
open Redux.Profunctor.Test.Mocks.Definitions

open Xunit

module Store =

    [<Fact>]
    let ``Scope | Scoping existing store doesnt change state`` () =
        let store = getNewStore ()
        let aFieldInitial = store.getState().DomainAState.AField1
        let scopedAStore = pscope store PFiber.domainAFiber
        let actual = scopedAStore.getState().AField1
        Assert.Equal(aFieldInitial, actual)

    [<Fact>]
    let ``Scope | Locally Scoped Store Actions affect Global state`` () =
        let store = getNewStore ()
        let scopedAStore = pscope store PFiber.domainAFiber
        scopedAStore.send (IncAField1 1)
        let globalActual = store.getState().DomainAState.AField1
        let scopedActual = scopedAStore.getState().AField1
        Assert.Equal(scopedActual, globalActual)

    [<Fact>]
    let ``DomainA | Received A Action`` () =
        let scopedAStore = pscope (getNewStore ()) PFiber.domainAFiber
        scopedAStore.send (IncAField1 1)
        let actual = scopedAStore.getState().AField1
        Assert.Equal(1, actual)

    [<Fact>]
    let ``DomainB | Received B Action`` () =
        let scopedBStore = pscope (getNewStore ()) PFiber.domainBFiber
        scopedBStore.send (IncBField1 1)
        let actual = scopedBStore.getState().BField1
        Assert.Equal(1, actual)

    [<Fact>]
    let ``DomainA | Received Multiple A Actions`` () =
        let scopedAStore = pscope (getNewStore ()) PFiber.domainAFiber
        scopedAStore.send (IncAField1 1)
        scopedAStore.send (IncAField1 1)
        let actual = scopedAStore.getState().AField1
        Assert.Equal(2, actual)

    [<Fact>]
    let ``SubA | AAction Should Trigger Effect`` () =
        let scopedAStore = pscope (getNewStore ()) PFiber.domainAFiber
        let mutable sideEffectTarget = 0
        scopedAStore.subscribe (fun domainAState -> sideEffectTarget <- domainAState.AField1)
        |> ignore
        scopedAStore.send (IncAField1 1)
        Assert.Equal(1, sideEffectTarget)

    [<Fact>]
    let ``SubA | AAction Should Trigger Multiple`` () =
        let scopedAStore = pscope (getNewStore ()) PFiber.domainAFiber
        let mutable sideEffectTarget = 0
        scopedAStore.subscribe (fun s -> sideEffectTarget <- sideEffectTarget + s.AField1)
        |> ignore
        scopedAStore.subscribe (fun s -> sideEffectTarget <- sideEffectTarget + s.AField1)
        |> ignore
        scopedAStore.send (IncAField1 1)
        Assert.Equal(2, sideEffectTarget)

    [<Fact>]
    let ``SubA | Disposed Should Not Trigger`` () =
        let scopedAStore = pscope (getNewStore ()) PFiber.domainAFiber
        let mutable sideEffectTarget = 0
        let disposable =
            scopedAStore.subscribe (fun s -> sideEffectTarget <- sideEffectTarget + s.AField1)
        disposable.Dispose()
        scopedAStore.send (IncAField1 1)
        Assert.Equal(0, sideEffectTarget)

    [<Fact>]
    let ``DomainA action does not affect DomainB state`` () =
        let store = getNewStore ()
        store.send (DomainAAction (IncAField1 5))
        let bState = store.getState().DomainBState
        Assert.Equal(0, bState.BField1)
        Assert.Equal(0, bState.BField2)

    [<Fact>]
    let ``DomainB action does not affect DomainA state`` () =
        let store = getNewStore ()
        store.send (DomainBAction (IncBField1 5))
        let aState = store.getState().DomainAState
        Assert.Equal(0, aState.AField1)
        Assert.Equal(0, aState.AField2)
