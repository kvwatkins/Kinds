module Redux.Demo.Counter

open Redux

type CounterState = { Value: int }
    with static member Zero = { Value = 0 }

type CounterAction =
    | Increment of int
    | Decrement of int
    | Reset

type CounterEnvironment = { ResetValue: int }

// When a Decrement would push the value below zero the reducer emits a Reset
// effect rather than going negative.  The store re-dispatches it automatically,
// driving the value back to ResetValue without any external orchestration.
let counterReducer : Reducer<CounterState, CounterAction, CounterEnvironment> =
    Reducer (fun state action env ->
        match action with
        | Increment n ->
            ({ state with Value = state.Value + n }, Effect.empty)
        | Decrement n ->
            let newVal = state.Value - n
            if newVal < 0
            then (state, Effect.pure' Reset)
            else ({ state with Value = newVal }, Effect.empty)
        | Reset ->
            ({ state with Value = env.ResetValue }, Effect.empty))
