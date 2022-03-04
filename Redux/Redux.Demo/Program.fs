module Redux.Demo.Program

open Redux
open Counter
open Logger
open App

// ─── Formatting helpers ──────────────────────────────────────────────────────

let banner (title: string) =
    let bar = String.replicate 70 "═"
    printfn ""
    printfn "%s" bar
    printfn "  %s" title
    printfn "%s" bar

let section (title: string) =
    printfn ""
    printfn "  ┌─ %s" title
    printfn "  │"

let note (msg: string)    = printfn "  │  %s" msg
let blank ()              = printfn "  │"
let result (msg: string)  = printfn "  │    → %s" msg
let close ()              = printfn "  └─────────────────────────────────────────"

let printCounter (label: string) (s: CounterState) =
    printfn "  │    %s: value=%d" label s.Value

let printLogger (label: string) (s: LoggerState) =
    let entries =
        if s.Entries.IsEmpty then "(empty)"
        else s.Entries |> String.concat " | "
    printfn "  │    %s: [%s]" label entries

// ─────────────────────────────────────────────────────────────────────────────
banner "Redux — Composable State Management"

printfn """
  SCENARIO
  ────────
  An app with two independent sub-systems:

    CounterDomain — tracks a numeric counter value
    LoggerDomain  — keeps a bounded log of messages

  Neither domain knows the other exists.

  Goal: compose them into one global reducer and store without
  changing either domain's code.
"""

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 1 — The raw reducers (domain-local)"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  counterReducer : Reducer<CounterState, CounterAction, CounterEnvironment>
  loggerReducer  : Reducer<LoggerState,  LoggerAction,  LoggerEnvironment>

  Each reducer runs in its own coordinate space.
"""

section "counterReducer in isolation"
let cEnv = { ResetValue = 0 }
let (c1, _) = counterReducer.run CounterState.Zero (Increment 5) cEnv
let (c2, _) = counterReducer.run c1                (Increment 3) cEnv
let (c3, _) = counterReducer.run c2                (Decrement 4) cEnv
note "  counterReducer.run Zero (Increment 5)"
printCounter "after Increment 5" c1
note "  counterReducer.run ...  (Increment 3)"
printCounter "after Increment 3" c2
note "  counterReducer.run ...  (Decrement 4)"
printCounter "after Decrement 4" c3
close ()

section "loggerReducer in isolation"
let lEnv = { MaxEntries = 3 }
let (l1, _) = loggerReducer.run LoggerState.Empty (AddEntry "app started")    lEnv
let (l2, _) = loggerReducer.run l1                (AddEntry "user logged in") lEnv
note "  loggerReducer.run Empty (AddEntry \"app started\")"
printLogger "after first entry " l1
note "  loggerReducer.run ...   (AddEntry \"user logged in\")"
printLogger "after second entry" l2
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 2 — Fiber.pullback : project into global coordinate space"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  Fiber.pullback lifts a local Reducer<'s,'a,'r> to Reducer<'t,'b,'u> using:

    • fiber.state       — a Lens'<'t,'s>  to read/write the state slice
    • fiber.action      — a Prism'<'b,'a> to route incoming actions
    • fiber.environment — a projection 'u -> 'r

  The local reducer code is completely unchanged.
"""

section "appReducer — composed reducer, run manually"
printCounter "Initial Counter" initialState.Counter
printLogger  "Initial Logger " initialState.Logger
blank ()
let (s1, _) = appReducer.run initialState (CounterMsg (Increment 10)) appEnv
note "  After CounterMsg (Increment 10):"
printCounter "  Counter" s1.Counter
printLogger  "  Logger " s1.Logger
blank ()
let (s2, _) = appReducer.run s1 (LoggerMsg (AddEntry "counter incremented")) appEnv
note "  After LoggerMsg (AddEntry \"counter incremented\"):"
printCounter "  Counter" s2.Counter
printLogger  "  Logger " s2.Logger
blank ()
note "  ✓ Each domain only reacts to its own action; the other is untouched."
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 3 — createStore and subscriptions"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  createStore wraps the reducer in a mutable cell and a subscriber map.
  subscribe returns an IDisposable — Dispose() removes the subscription.
"""

let store    = createStore appReducer initialState appEnv
let globalLog = System.Collections.Generic.List<AppState>()
let _sub      = store.subscribe (fun s -> globalLog.Add s)

section "Global store — sends and subscriptions"
note "  store.send (CounterMsg (Increment 7))"
store.send (CounterMsg (Increment 7))
note "  store.send (LoggerMsg  (AddEntry \"hello\"))"
store.send (LoggerMsg (AddEntry "hello"))
note "  store.send (CounterMsg (Decrement 2))"
store.send (CounterMsg (Decrement 2))
blank ()
note (sprintf "  globalLog received %d notifications:" globalLog.Count)
for s in globalLog do
    printfn "  │      counter=%-4d  log=[%s]"
        s.Counter.Value
        (if s.Logger.Entries.IsEmpty then "" else s.Logger.Entries |> String.concat ", ")
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 4 — Fiber.scope : a focused window onto the global store"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  Fiber.scope creates a Store<'s,'a,'r> that is a focused view into the
  global Store<'t,'b,'u>:

    • getState  — extracts the local slice via fiber.state.get
    • send      — wraps the local action via fiber.action.build before forwarding
    • subscribe — maps global notifications through fiber.state.get

  The Counter component never sees LoggerState or AppAction, and vice versa.
"""

let store2       = createStore appReducer initialState appEnv
let counterScope : Store<CounterState, CounterAction, CounterEnvironment> = Fiber.scope store2 counterFiber
let loggerScope  : Store<LoggerState,  LoggerAction,  LoggerEnvironment>  = Fiber.scope store2 loggerFiber

let counterLog = System.Collections.Generic.List<CounterState>()
let loggerLog  = System.Collections.Generic.List<LoggerState>()
let _csub = counterScope.subscribe (fun cs -> counterLog.Add cs)
let _lsub = loggerScope.subscribe  (fun ls -> loggerLog.Add ls)

section "Scoped sends"
note "  counterScope.send (Increment 3)    [CounterAction — no AppAction in sight]"
counterScope.send (Increment 3)
note "  counterScope.send (Decrement 1)"
counterScope.send (Decrement 1)
note "  loggerScope.send  (AddEntry \"first log\")"
loggerScope.send  (AddEntry "first log")
blank ()
note (sprintf "  counterLog (%d entries — only CounterState, never AppState):" counterLog.Count)
for cs in counterLog do
    result (sprintf "CounterState.Value = %d" cs.Value)
blank ()
note (sprintf "  loggerLog (%d entries — only LoggerState, never AppState):" loggerLog.Count)
for ls in loggerLog do
    result (sprintf "LoggerState.Entries = [%s]" (ls.Entries |> String.concat ", "))
blank ()
note "  ✓ counterScope.getState() returns CounterState, not AppState."
note (sprintf "  ✓ counterScope.getState().Value = %d" (counterScope.getState().Value))
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 5 — Effect propagation : auto-reset and auto-clear"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  Two effects are baked into the domain reducers:

    counterReducer : Decrement that would go negative
                     → emits  Effect.pure' Reset
                     → store  re-dispatches CounterMsg Reset automatically

    loggerReducer  : AddEntry that exceeds MaxEntries (=%d)
                     → emits  Effect.pure' ClearLog
                     → store  re-dispatches LoggerMsg ClearLog automatically

  Neither domain knows about the store — it just returns an Effect.
""" appEnv.Logger.MaxEntries

section "Counter auto-reset — Decrement below zero"
let s3start : AppState = { Counter = { Value = 3 }; Logger = LoggerState.Empty }
let store3   = createStore appReducer s3start appEnv
let stateLog3 = System.Collections.Generic.List<AppState>()
let _s3sub   = store3.subscribe (fun s -> stateLog3.Add s)
note (sprintf "  Counter before: %d" (store3.getState().Counter.Value))
note         "  store3.send (CounterMsg (Decrement 10))   // 3 - 10 = -7 → emits Reset"
store3.send  (CounterMsg (Decrement 10))
blank ()
note (sprintf "  stateLog3 received %d notifications (guard + auto-Reset):" stateLog3.Count)
for s in stateLog3 do
    result (sprintf "counter = %d" s.Counter.Value)
note (sprintf "  Counter after (settled): %d" (store3.getState().Counter.Value))
blank ()
note "  ✓ Reset effect was dispatched and handled inside createStore."
note "  ✓ First notification: state unchanged (guard). Second: Reset applied."
close ()

section "Logger auto-clear — entries exceed MaxEntries"
let lStore    = createStore appReducer initialState appEnv
let lStateLog = System.Collections.Generic.List<AppState>()
let _lSub     = lStore.subscribe (fun s -> lStateLog.Add s)
note (sprintf "  MaxEntries = %d" appEnv.Logger.MaxEntries)
note         "  Sending 6 log entries..."
for i in 1..6 do
    lStore.send (LoggerMsg (AddEntry (sprintf "entry %d" i)))
blank ()
note (sprintf "  lStateLog received %d notifications (6 adds + auto-ClearLog):" lStateLog.Count)
for s in lStateLog do
    result (sprintf "entries = [%s]"
        (if s.Logger.Entries.IsEmpty then "(cleared)" else s.Logger.Entries |> String.concat ", "))
blank ()
note "  ✓ ClearLog effect fired automatically when log exceeded MaxEntries."
note "  ✓ Log is empty — bounded without any external orchestration."
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "SUMMARY"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  The Redux library provides four building blocks:

    Effect<'a>               — a deferred callback computation.
                               Combined with (+); re-dispatched by the store.

    Reducer<'s,'a,'r>        — pure (state, action, env) → (state, effect).
                               Composed sequentially with >.>

    Store<'s,'a,'r>          — the live mutable cell.
                               Built with createStore.
                               Exposes getState / send / subscribe.

    Fiber<'t,'s,'b,'a,'u,'r> — the three optics that describe how a local
                               domain embeds inside a global one:
                                   state       : Lens'<'t,'s>
                                   action      : Prism'<'b,'a>
                                   environment : 'u -> 'r
                               Fiber.pullback lifts a Reducer to global scope.
                               Fiber.scope    projects a Store down to local.

  The key insight: pullback is an optics-based coordinate change.
  Each domain's Reducer, Effect, and Environment stay completely isolated.
  Composition happens at the app boundary via Fiber — no leakage either way.
"""
