namespace Redux.Test.Unit

module Effect =
    open Xunit
    open Redux.Effect

    (* --- Effect --- *)
    module Invariants =
        // Monoid laws
        [<Fact>]
        let ``Monoid | Identity *by Type and Parametric Constraint only`` () =
            let mutable sideEffect = false

            let isEqual =
                (Effect(fun f -> sideEffect <- true)) = (Effect(fun f -> sideEffect <- true))

            Assert.False(isEqual)

(*        [<Fact>]
        let ``Monoid | Associativity`` () =
            let mutable sideEffect = 0
            let mutable sideEffect2 = 0

            (Effect(fun _ -> sideEffect <- 5) +
                Effect(fun _ -> sideEffect <- sideEffect + -3)) +
                Effect(fun _ -> sideEffect <- sideEffect + 1) |> Effect.ignoreThenRun

            Effect(fun _ -> sideEffect2 <- 5) +
               (Effect(fun _ -> sideEffect2 <- sideEffect2 + -3) +
                Effect(fun _ -> sideEffect2 <- sideEffect2 + 1)) |> Effect.ignoreThenRun
            Assert.True((sideEffect = sideEffect2))

        [<Fact>]
        let ``Runs with Left Associativity`` () =
            let mutable sideEffect = -1
            (Effect(fun _ -> sideEffect <- sideEffect * 2)) +
            (Effect(fun _ -> sideEffect <- sideEffect * -5))

            Assert.True((sideEffect = 10))*)
