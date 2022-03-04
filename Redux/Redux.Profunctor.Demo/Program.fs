module Redux.Profunctor.Demo.Program

open Redux
open Redux.Profunctor
open Kinds.Data.Either
open Kinds.Control.Lens.Profunctor    // lensToP, prismToP
open Counter
open Logger
open App

// ─── Formatting helpers ───────────────────────────────────────────────────────

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

// ─── Demo items ───────────────────────────────────────────────────────────────

banner "Architecture.Profunctor — Composable State Management"

printfn """
  SCENARIO
  ────────
  An app with two independent sub-systems:

    CounterDomain — tracks a numeric counter value
    LoggerDomain  — keeps a bounded log of messages

  Neither domain knows the other exists.

  Goal: compose them into one global reducer without changing
  either domain's code.
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
note "  counterReducer.run Zero (Increment 5) cEnv"
printCounter "after Increment 5" c1
note "  counterReducer.run ...  (Increment 3) cEnv"
printCounter "after Increment 3" c2
note "  counterReducer.run ...  (Decrement 4) cEnv"
printCounter "after Decrement 4" c3
close ()

section "loggerReducer in isolation"
let lEnv = { MaxEntries = 3 }
let (l1, _) = loggerReducer.run LoggerState.Empty (AddEntry "app started")    lEnv
let (l2, _) = loggerReducer.run l1                (AddEntry "user logged in") lEnv
note "  loggerReducer.run Empty (AddEntry \"app started\") lEnv"
printLogger "after first entry " l1
note "  loggerReducer.run ...   (AddEntry \"user logged in\") lEnv"
printLogger "after second entry" l2
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 2 — magnify : project the environment"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  Problem: counterReducer needs CounterEnvironment but the app has AppEnvironment.

  magnify (fun (e: AppEnvironment) -> e.Counter) counterReducer
    : Reducer<CounterState, CounterAction, AppEnvironment>

  The reducer logic is completely unchanged.  magnify just pre-applies
  the projection function before the environment reaches the reducer.
  No knowledge of AppEnvironment leaks into the counter domain.
"""

let magnifiedCounterReducer : Reducer<CounterState, CounterAction, AppEnvironment> =
    magnify (fun (e: AppEnvironment) -> e.Counter) counterReducer

section "magnifiedCounterReducer — same logic, wider environment"
let (magC1, _) = magnifiedCounterReducer.run CounterState.Zero (Increment 7) appEnv
note "  magnifiedCounterReducer.run Zero (Increment 7) appEnv"
printCounter "CounterState" magC1
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 3 — zoom : focus the state coordinate"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  Problem: magnifiedCounterReducer still works on CounterState, but the app
  has AppState = { Counter: CounterState; Logger: LoggerState }.

  zoom takes a profunctor optic  H2<p,CounterState,CounterState> → H2<p,AppState,AppState>
  and applies it to the reducer:

  zoom (lensToP counterStateLens) magnifiedCounterReducer
    : Reducer<AppState, CounterAction, AppEnvironment>

  Internally zoom uses the Cartesian profunctor instance (first + dimap)
  on ReducerStatePEncoding to thread the "outer" AppState through the
  reducer, updating only the Counter slice.
"""

let counterStateLens : Kinds.Control.Lens.Concrete.Lens<AppState, CounterState> =
    Kinds.Control.Lens.Concrete.Lens(
        (fun s   -> s.Counter),
        (fun s c -> { s with Counter = c }))

let zoomedCounterReducer : Reducer<AppState, CounterAction, AppEnvironment> =
    zoom (lensToP counterStateLens) magnifiedCounterReducer

section "zoomedCounterReducer — Counter slice updated, Logger untouched"
let startState : AppState = { Counter = CounterState.Zero; Logger = LoggerState.Empty }
printLogger "Logger before" startState.Logger
let (zoomed1, _) = zoomedCounterReducer.run startState (Increment 5) appEnv
printCounter "Counter after Increment 5" zoomed1.Counter
printLogger  "Logger  after Increment 5" zoomed1.Logger
blank ()
note "  ✓ Logger slice is identical before and after"
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 4 — zoomAction : route the action coordinate"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  Problem: zoomedCounterReducer handles CounterAction but the app dispatches
  AppAction = CounterMsg of CounterAction | LoggerMsg of LoggerAction.

  zoomAction takes a profunctor optic  H2<p,CounterAction,CounterAction> → H2<p,AppAction,AppAction>
  and applies it to the reducer:

  zoomAction (prismToP counterActionPrism) zoomedCounterReducer
    : Reducer<AppState, AppAction, AppEnvironment>

  Internally zoomAction uses the CoCartesian profunctor instance (right_ + dimap)
  on ReducerActionPEncoding.  A CounterMsg is unwrapped and dispatched; any other
  AppAction variant returns (state, Effect.empty) — it is silently ignored.
  Effects emitted by the inner reducer are re-wrapped as CounterMsg via embedAction.
"""

let counterActionPrism : Kinds.Control.Lens.Concrete.Prism<AppAction, CounterAction> =
    Kinds.Control.Lens.Concrete.Prism(
        CounterMsg,
        fun a -> match a with CounterMsg ca -> Right ca | _ -> Left a)

let actionZoomedCounterReducer : Reducer<AppState, AppAction, AppEnvironment> =
    zoomAction (prismToP counterActionPrism) zoomedCounterReducer

section "actionZoomedCounterReducer — CounterMsg dispatched, LoggerMsg ignored"
let startState2 : AppState = { Counter = CounterState.Zero; Logger = LoggerState.Empty }
let (az1, _) = actionZoomedCounterReducer.run startState2 (CounterMsg (Increment 10)) appEnv
note "  Send AppAction (CounterMsg (Increment 10)):"
printCounter "Counter" az1.Counter
blank ()
let (az2, _) = actionZoomedCounterReducer.run startState2 (LoggerMsg (AddEntry "ignored")) appEnv
note "  Send AppAction (LoggerMsg (AddEntry \"ignored\")):"
printCounter "Counter" az2.Counter
result "Counter unchanged  ✓  (LoggerMsg is not a CounterMsg — silently ignored)"
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 5 — ppullback : all three steps composed"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  ppullback does Steps 2–4 in one call:

    ppullback counterReducer counterFiber
      =  magnify fiber.environment        -- Reducer<CounterState,CounterAction,AppEnvironment>
      |> zoom       (lensOptic  fiber)    -- Reducer<AppState, CounterAction,AppEnvironment>
      |> zoomAction (prismOptic fiber)    -- Reducer<AppState, AppAction, AppEnvironment>

  Composing two ppullbacks with >.> gives the full app reducer:

    ppullback counterReducer counterFiber
    >.> ppullback loggerReducer loggerFiber
      : Reducer<AppState, AppAction, AppEnvironment>

  Every AppAction flows through both reducers in sequence.  Each one
  either handles or ignores the action independently.
"""

section "Direct use — run the composed reducer manually"
let s0 : AppState = initialState
printCounter "Initial Counter" s0.Counter
printLogger  "Initial Logger " s0.Logger
blank ()

let (s1, _) = appReducer.run s0 (CounterMsg (Increment 10)) appEnv
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

section "Effect propagation — Decrement triggers automatic Reset"
printfn """  │  counterReducer emits  Effect.pure' Reset
  │  when a Decrement would push the value below zero.
  │
  │  After ppullback that becomes  Effect.pure' (CounterMsg Reset).
  │  The store re-dispatches it automatically — no external orchestration needed.
"""
let preState : AppState = { Counter = { Value = 3 }; Logger = LoggerState.Empty }
printCounter "  Before     " preState.Counter

// Decrement by 10 — value would go to -7, so Reset fires.
let (afterDecrement, resetEffect) = appReducer.run preState (CounterMsg (Decrement 10)) appEnv
printCounter "  After Decrement ×10 (3→-7, guard holds, effect queued)" afterDecrement.Counter

// Simulate the store dispatching the effect back into the reducer.
// (In the live store this happens automatically inside createStore.)
let mutable effectState = afterDecrement
resetEffect.run (fun action ->
    note (sprintf "  Effect dispatched: %A" action)
    let (s, _) = appReducer.run effectState action appEnv
    effectState <- s)
printCounter "  After auto-Reset" effectState.Counter
blank ()
note "  ✓ Effect crossed the domain boundary as AppAction automatically."
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 6 — pscope : a focused window onto the global store"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  The global store holds AppState and accepts AppAction.
  A Counter UI component should only see CounterState and send CounterAction.

  pscope globalStore counterFiber : Store<CounterState, CounterAction, CounterEnvironment>

  The scoped store:
    • getState  — calls fiber.getState to extract CounterState from AppState
    • send      — wraps CounterAction via fiber.embedAction before forwarding
    • subscribe — maps AppState notifications through fiber.getState

  The Counter component is completely unaware of Logger or AppState.
"""

let globalStore = createStore appReducer initialState appEnv

section "Live store — subscriptions and scoped sends"

let globalLog = System.Collections.Generic.List<AppState>()
let counterLog = System.Collections.Generic.List<CounterState>()

let globalSub   = globalStore.subscribe (fun s  -> globalLog.Add s)
let counterScope = pscope globalStore counterFiber
let counterSub  = counterScope.subscribe (fun cs -> counterLog.Add cs)

note "  Subscribed to both global store and counter scope."
blank ()

note "  counterScope.send (Increment 5)      [CounterAction, no AppAction in sight]"
counterScope.send (Increment 5)

note "  counterScope.send (Increment 3)"
counterScope.send (Increment 3)

note "  counterScope.send (Decrement 2)"
counterScope.send (Decrement 2)

blank ()
note (sprintf "  globalLog received %d notifications (one per send):" globalLog.Count)
for s in globalLog do
    printfn "  │      counter=%-4d  log=[%s]"
        s.Counter.Value
        (if s.Logger.Entries.IsEmpty then "" else s.Logger.Entries |> String.concat ", ")

blank ()
note (sprintf "  counterLog received %d notifications (only Counter slice):" counterLog.Count)
for cs in counterLog do
    printCounter "CounterState" cs

blank ()
note "  ✓ Counter scope subscriber never saw AppState or LoggerState."
note "  ✓ Global subscriber saw every state change caused by counter actions."

// Send a logger action through the global store.  pscope subscribes to all
// global notifications so the counter scope fires too — but the CounterState
// value it receives is the same as before.
let counterValueBefore = counterScope.getState().Value
globalStore.send (LoggerMsg (AddEntry "background log"))
let counterValueAfter  = counterScope.getState().Value

blank ()
note "  globalStore.send (LoggerMsg (AddEntry \"background log\"))"
result (sprintf "global notifications total      : %d" globalLog.Count)
result (sprintf "counter scope notifications     : %d (fires on every global change)" counterLog.Count)
result (sprintf "counter value before / after    : %d / %d (unchanged)" counterValueBefore counterValueAfter)
note "  ✓ Counter scope subscribers always receive CounterState, never AppState."
note "  ✓ CounterState.Value did not change — Logger action has no effect on Counter."

globalSub.Dispose()
counterSub.Dispose()
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "STEP 6b — pscope and effects stay in sync across domains"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  Demonstrate the effect loop from Step 5 inside a live store,
  confirming that the Reset effect re-enters the store and
  both global and scoped subscribers see the final settled state.
"""

let store2initState : AppState = { Counter = { Value = 3 }; Logger = LoggerState.Empty }
let store2 = createStore appReducer store2initState appEnv

let counterScope2 = pscope store2 counterFiber

let globalLog2  = System.Collections.Generic.List<AppState>()
let counterLog2 = System.Collections.Generic.List<CounterState>()

let _gsub2 = store2.subscribe       (fun s  -> globalLog2.Add s)
let _csub2 = counterScope2.subscribe (fun cs -> counterLog2.Add cs)

section "Decrement ×10 via counter scope — effect loop fires inside store"
note   (sprintf "  Counter before: %d" (store2.getState().Counter.Value))
counterScope2.send (Decrement 10)
note   (sprintf "  Counter after (settled): %d" (store2.getState().Counter.Value))
blank ()
note   (sprintf "  globalLog2 received %d notifications (guard + auto-Reset)" globalLog2.Count)
for s in globalLog2 do
    result (sprintf "Counter value = %d" s.Counter.Value)
blank ()
note "  ✓ Reset effect was dispatched and handled inside createStore."
note "  ✓ Counter scope subscriber saw both the guard state and the recovery."
close ()

// ─────────────────────────────────────────────────────────────────────────────
banner "SUMMARY"
// ─────────────────────────────────────────────────────────────────────────────

printfn """
  The three zoom combinators each change exactly one type coordinate:

    magnify  proj  r  :  Reducer<'s,'a,'r>  →  Reducer<'s,'a,'u>   (env  widened)
    zoom     optic r  :  Reducer<'s,'a,'r>  →  Reducer<'t,'a,'r>   (state widened)
    zoomAction optic r:  Reducer<'s,'a,'r>  →  Reducer<'s,'b,'r>   (action widened)

  ppullback composes all three.  A PFiber is just the raw functions the
  compiler needs — no optics library required on the Architecture.Profunctor side.

  pscope inverts the lens at the store level, giving any sub-component a
  fully-typed local Store without it knowing the global type exists.

  The key insight: pullback is not a monolithic operation.  It is the
  natural composition of three independent, orthogonal coordinate changes,
  each justified by a different profunctor constraint:

    zoom       ←→  Cartesian   (Strong)    — products / Lens
    zoomAction ←→  CoCartesian (Choice)    — sums     / Prism
    magnify    ←→  plain contravariance    — functions (no profunctor needed)
"""
