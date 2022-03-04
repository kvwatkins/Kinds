module Redux.Demo.App

open Prelude
open Redux
open Optics
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

// ─── Fibers ───────────────────────────────────────────────────────────────────
//
// A Fiber bundles the three optics needed to pull a local reducer back to
// the global one:
//   • state       — Lens'<AppState, 's>  to read/write the state slice
//   • action      — Prism'<AppAction, 'a> to route incoming actions
//   • environment — AppEnvironment -> 'r  to extract the local environment

let counterFiber : Fiber<AppState, CounterState, AppAction, CounterAction, AppEnvironment, CounterEnvironment> =
    { state       = { get = fun s -> s.Counter
                      set = fun s c -> { s with Counter = c } }
      action      = { build  = CounterMsg
                      match_ = fun a ->
                          match a with
                          | CounterMsg ca -> Right ca
                          | _             -> Left  a }
      environment = fun e -> e.Counter }

let loggerFiber : Fiber<AppState, LoggerState, AppAction, LoggerAction, AppEnvironment, LoggerEnvironment> =
    { state       = { get = fun s -> s.Logger
                      set = fun s l -> { s with Logger = l } }
      action      = { build  = LoggerMsg
                      match_ = fun a ->
                          match a with
                          | LoggerMsg la -> Right la
                          | _            -> Left  a }
      environment = fun e -> e.Logger }

// ─── Global Reducer ───────────────────────────────────────────────────────────
//
// Fiber.pullback lifts each local reducer to the global coordinate space.
// >.> sequences them so every action passes through both reducers in order.
// Each reducer handles its own action variant and returns (state, Effect.empty)
// for any action it does not own.

let appReducer : Reducer<AppState, AppAction, AppEnvironment> =
    Fiber.pullback counterReducer counterFiber
    >.> Fiber.pullback loggerReducer loggerFiber
