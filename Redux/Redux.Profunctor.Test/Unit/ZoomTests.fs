namespace Redux.Profunctor.Test.Unit

open Xunit
open Redux
open Redux.Profunctor
open Kinds.Data.Either
open Kinds.Control.Lens.Profunctor

module ZoomTests =

    // ---------------------------------------------------------------------------
    // Helpers: a small two-field record as the "outer" state
    // ---------------------------------------------------------------------------

    type OuterState = { Inner: int; Other: string }

    let innerLens : Kinds.Control.Lens.Concrete.Lens<OuterState, int> =
        Kinds.Control.Lens.Concrete.Lens(
            (fun s -> s.Inner),
            (fun s v -> { s with Inner = v }))

    let innerReducer : Reducer<int, int, unit> =
        Reducer (fun s a _ -> (s + a, Effect.empty))

    // ---------------------------------------------------------------------------
    // zoom tests
    // ---------------------------------------------------------------------------

    [<Fact>]
    let ``zoom | updates sub-state and leaves remainder unchanged`` () =
        let outerReducer = zoom (lensToP innerLens) innerReducer
        let initial = { Inner = 0; Other = "hello" }
        let (result, _) = outerReducer.run initial 5 ()
        Assert.Equal(5, result.Inner)
        Assert.Equal("hello", result.Other)

    [<Fact>]
    let ``zoom | accumulates correctly over multiple runs`` () =
        let outerReducer = zoom (lensToP innerLens) innerReducer
        let initial = { Inner = 0; Other = "x" }
        let (s1, _) = outerReducer.run initial 3 ()
        let (s2, _) = outerReducer.run s1 4 ()
        Assert.Equal(7, s2.Inner)

    [<Fact>]
    let ``zoom | composes correctly via Reducer sequential composition`` () =
        let outerReducer1 = zoom (lensToP innerLens) innerReducer
        let outerReducer2 = zoom (lensToP innerLens) innerReducer
        let combined = outerReducer1 >.> outerReducer2
        let initial = { Inner = 0; Other = "x" }
        let (result, _) = combined.run initial 3 ()
        Assert.Equal(6, result.Inner) // 0 + 3 + 3

    // ---------------------------------------------------------------------------
    // zoomAction tests
    // ---------------------------------------------------------------------------

    type OuterAction =
        | InnerA of int
        | OtherAction

    let innerPrism : Kinds.Control.Lens.Concrete.Prism<OuterAction, int> =
        Kinds.Control.Lens.Concrete.Prism(
            InnerA,
            fun s ->
                match s with
                | InnerA a -> Right a
                | _ -> Left s)

    let actionReducer : Reducer<int, int, unit> =
        Reducer (fun s a _ -> (s + a, Effect.empty))

    [<Fact>]
    let ``zoomAction | dispatches matching action`` () =
        let outerReducer = zoomAction (prismToP innerPrism) actionReducer
        let (result, _) = outerReducer.run 0 (InnerA 5) ()
        Assert.Equal(5, result)

    [<Fact>]
    let ``zoomAction | returns unchanged state on mismatch`` () =
        let outerReducer = zoomAction (prismToP innerPrism) actionReducer
        let (result, _) = outerReducer.run 42 OtherAction ()
        Assert.Equal(42, result)

    [<Fact>]
    let ``zoomAction | returns Effect.empty on mismatch`` () =
        let mutable fired = false
        let outerReducer = zoomAction (prismToP innerPrism) actionReducer
        let (_, eff) = outerReducer.run 0 OtherAction ()
        eff.run (fun _ -> fired <- true)
        Assert.False(fired)

    [<Fact>]
    let ``zoomAction | maps effects back via prism embed`` () =
        let captured = System.Collections.Generic.List<OuterAction>()
        let innerWithEffect : Reducer<int, int, unit> =
            Reducer (fun s a _ -> (s + a, Effect.pure' (a * 10)))
        let outerReducer = zoomAction (prismToP innerPrism) innerWithEffect
        let (_, eff) = outerReducer.run 0 (InnerA 3) ()
        eff.run (fun action -> captured.Add(action))
        Assert.Equal(1, captured.Count)
        Assert.Equal(InnerA 30, captured[0])

    // ---------------------------------------------------------------------------
    // magnify tests
    // ---------------------------------------------------------------------------

    type BigEnv = { Inner: unit; Extra: string }

    [<Fact>]
    let ``magnify | projects larger env to smaller type the reducer expects`` () =
        let smallReducer : Reducer<int, int, unit> =
            Reducer (fun s a _ -> (s + a, Effect.empty))
        let largeReducer = magnify (fun (e: BigEnv) -> e.Inner) smallReducer
        let (result, _) = largeReducer.run 0 5 { Inner = (); Extra = "ignored" }
        Assert.Equal(5, result)

    [<Fact>]
    let ``magnify | only projects env, does not change state or action behavior`` () =
        let smallReducer : Reducer<int, int, unit> =
            Reducer (fun s a _ -> (s * a, Effect.empty))
        let largeReducer = magnify (fun (e: BigEnv) -> e.Inner) smallReducer
        let (result, _) = largeReducer.run 3 7 { Inner = (); Extra = "ignored" }
        Assert.Equal(21, result)
