namespace Redux.Test.Integration

open Redux
open Redux.Test.Mocks.Implementations.MockApp
open Redux.Test.Mocks.Implementations
open Redux.Test.Mocks.Definitions

open Xunit

module Store =
    [<Fact>]
    let ``Scope | Scoping existing store doesnt change state`` () =
        let store = getNewStore ()
        let aFieldInitial = store.getState().DomainAState.AField1
        let scopedAStore = scope store Fiber.domainAFiber
        let actual = scopedAStore.getState().AField1

        Assert.Equal(aFieldInitial, actual)

    [<Fact>]
    let ``Scope | Locally Scoped Store Actions affect Global state`` () =
        let store = getNewStore ()

        let scopedAStore = scope store Fiber.domainAFiber
        scopedAStore.send (IncAField1 1)

        let globalActual = store.getState().DomainAState.AField1
        let scopedActual = scopedAStore.getState().AField1

        Assert.Equal(scopedActual, globalActual)

    [<Fact>]
    let ``DomainA | Received A Action`` () =
        let scopedAStore =
            scope (getNewStore ()) Fiber.domainAFiber

        scopedAStore.send (IncAField1 1)

        let actual = scopedAStore.getState().AField1

        Assert.Equal(1, actual)

    [<Fact>]
    let ``DomainB | Received B Action`` () =
        let scopedBStore =
            scope (getNewStore ()) Fiber.domainBFiber

        scopedBStore.send (IncBField1 1)

        let actual = scopedBStore.getState().BField1

        Assert.Equal(1, actual)

    [<Fact>]
    let ``DomainA | Received Multiple A Actions`` () =
        let scopedAStore =
            scope (getNewStore ()) Fiber.domainAFiber

        scopedAStore.send (IncAField1 1)
        scopedAStore.send (IncAField1 1)

        let actual = scopedAStore.getState().AField1

        Assert.Equal(2, actual)

    [<Fact>]
    let ``SubA | AAction Should Trigger Effect`` () =
        let scopedAStore =
            scope (getNewStore ()) Fiber.domainAFiber

        let mutable sideEffectTarget = 0

        scopedAStore.subscribe (fun domainAState -> sideEffectTarget <- domainAState.AField1)
        |> ignore

        scopedAStore.send (IncAField1 1)

        Assert.Equal(1, sideEffectTarget)

    [<Fact>]
    let ``SubA | AAction Should Trigger Multiple`` () =
        let scopedAStore =
            scope (getNewStore ()) Fiber.domainAFiber

        let mutable sideEffectTarget = 0

        scopedAStore.subscribe (fun domainAState -> sideEffectTarget <- sideEffectTarget + domainAState.AField1)
        |> ignore

        scopedAStore.subscribe (fun domainAState -> sideEffectTarget <- sideEffectTarget + domainAState.AField1)
        |> ignore

        scopedAStore.send (IncAField1 1)

        Assert.Equal(2, sideEffectTarget)

    [<Fact>]
    let ``SubA | Disposed Should Not Trigger`` () =
        let scopedAStore =
            scope (getNewStore ()) Fiber.domainAFiber

        let mutable sideEffectTarget = 0

        let disposable =
            scopedAStore.subscribe (fun domainAState -> sideEffectTarget <- sideEffectTarget + domainAState.AField1)

        disposable.Dispose()

        scopedAStore.send (IncAField1 1)

        Assert.Equal(0, sideEffectTarget)
