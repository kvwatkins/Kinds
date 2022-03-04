module Redux.Demo.Logger

open Redux

type LoggerState = { Entries: string list }
    with static member Empty = { Entries = [] }

type LoggerAction =
    | AddEntry of string
    | ClearLog

type LoggerEnvironment = { MaxEntries: int }

// When the number of log entries exceeds MaxEntries the reducer emits a ClearLog
// effect.  The store dispatches it automatically, keeping the log bounded
// without any external orchestration.
let loggerReducer : Reducer<LoggerState, LoggerAction, LoggerEnvironment> =
    Reducer (fun state action env ->
        match action with
        | AddEntry msg ->
            let entries = state.Entries @ [msg]
            let effect =
                if entries.Length > env.MaxEntries
                then Effect.pure' ClearLog
                else Effect.empty
            ({ state with Entries = entries }, effect)
        | ClearLog ->
            ({ state with Entries = [] }, Effect.empty))
