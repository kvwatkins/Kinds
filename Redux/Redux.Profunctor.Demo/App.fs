module Redux.Profunctor.Demo.App

open Redux
open Redux.Profunctor
open Counter
open Logger

// ─── Global Types ─────────────────────────────────────────────────────────────
//
// The "app shell" combines both domains into a single product/sum.
// Neither Counter nor Logger know these types exist.

type AppState =
    { Counter: CounterState
      Logger:  LoggerState }

type AppAction =
    | CounterMsg of CounterAction
    | LoggerMsg  of LoggerAction

type AppEnvironment =
    { Counter: CounterEnvironment
      Logger:  LoggerEnvironment }

// ─── Starting values ──────────────────────────────────────────────────────────

let initialState : AppState =
    { Counter = CounterState.Zero
      Logger  = LoggerState.Empty }

let appEnv : AppEnvironment =
    { Counter = { ResetValue = 0 }
      Logger  = { MaxEntries = 5 } }

// ─── PFibers ──────────────────────────────────────────────────────────────────
//
// A PFiber bundles the four raw functions needed to pull a local reducer back to
// the global one.  No optics library dependency required here — just plain F# functions.

let counterFiber : PFiber<AppState, CounterState, AppAction, CounterAction, AppEnvironment, CounterEnvironment> =
    { getState    = fun s -> s.Counter
      setState    = fun s c -> { s with Counter = c }
      matchAction = fun a -> match a with CounterMsg ca -> Some ca | _ -> None
      embedAction = CounterMsg
      environment = fun e -> e.Counter }

let loggerFiber : PFiber<AppState, LoggerState, AppAction, LoggerAction, AppEnvironment, LoggerEnvironment> =
    { getState    = fun s -> s.Logger
      setState    = fun s l -> { s with Logger = l }
      matchAction = fun a -> match a with LoggerMsg la -> Some la | _ -> None
      embedAction = LoggerMsg
      environment = fun e -> e.Logger }

// ─── Global Reducer ───────────────────────────────────────────────────────────
//
// ppullback lifts each local reducer to the global coordinate space, then
// >.> sequences them: every action flows through counter first, then logger.
// Each reducer only reacts to its own action variant; the other is a no-op.

let appReducer : Reducer<AppState, AppAction, AppEnvironment> =
    ppullback counterReducer counterFiber
    >.> ppullback loggerReducer loggerFiber
