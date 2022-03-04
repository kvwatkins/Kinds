# Reducer Architecture in F#

For many years I have been working with reducer architectures such as
[Redux](https://redux.js.org/) and [Elm](https://elm-lang.org/). I have
implemented several production solutions using these patterns in C++,
Objective-C, Swift, Kotlin, and F#. I have also worked directly with the
implementors of some of the open source solutions in this space.

Frameworks applying these patterns build upon a single common pattern
with variations for different purposes and goals. The properties that
most interest me are the compositionality of the approach and the
separation of side effects.

This is an F# port of the architecture explored in
[haskell-reducer](https://github.com/thomasmarsh/haskell-reducer).
The first half covers the core framework — `Effect`, `Reducer`, `Store`,
and `Fiber` pullback — translated directly from the Haskell.
The second half shows how `Redux.Profunctor` expands the pullback by
making its three orthogonal axes explicit using the
[Kinds](../README.md) higher-kinded type class system.

---

## Sink

A callback, in general, is pretty simple. We use them all the time in UI
code. In Haskell the type must be explicit about its `IO` context:

```haskell
type Sink a = a -> IO ()
```

In F# side-effects are implicit, so the type collapses to a plain
function. We don't give it its own alias, but conceptually every
`'a -> unit` callback in this codebase is a Sink. The Elmish equivalent
captures the intent:

```fsharp
type Dispatch<'msg> = 'msg -> unit
```

---

## Effect

An `Effect<'a>` is a continuation-like type. It holds a deferred
computation that, when run with a callback, invokes that callback zero
or more times with values of type `'a`. This lets reducers return
first-class, composable side effects rather than executing them directly.

```haskell
-- Haskell
newtype Effect a = Effect { runEffect :: Sink a -> IO () }
    deriving Functor
```

```fsharp
// F#
type Effect<'a>(run: ('a -> unit) -> unit) =
    member e.run = run
```

The Swift / Composable Architecture equivalent for comparison:

```swift
struct Effect<A> {
    let run: (@escaping (A) -> Void) -> Void
}
```

### Semigroup — combining effects

Two effects combine by running both under the same callback. This
matches Haskell's `Semigroup` instance exactly:

```haskell
instance Semigroup (Effect a) where
    Effect x <> Effect y = Effect $ \cb -> x cb >> y cb
```

```fsharp
static member (+)(x: Effect<'a>, y: Effect<'a>) =
    Effect(fun cb -> x.run cb; y.run cb)
```

### Monoid — the empty effect

```haskell
instance Monoid (Effect a) where
    mempty = Effect . const $ pure ()
```

```fsharp
static member empty: Effect<'a> = Effect(fun _ -> ())
```

### Lifting a value into an effect

`Effect.pure'` wraps a single value, equivalent to Haskell's
`Applicative.pure` for the `Cont` monad:

```haskell
instance Applicative Effect where
    pure = Effect . flip id
```

```fsharp
static member pure' a : Effect<'a> = Effect(fun cb -> cb a)
```

This is how reducers emit follow-up actions. Returning
`Effect.pure' Reset` causes the store to re-dispatch `Reset`
automatically after the current state transition — no external
orchestration needed.

### Void effects

`Effect.runToVoid` converts a fire-and-forget `Effect<Never>` into a
typed `Effect<'b>`. The `Never` type (`type Never = private Void of
Never`) ensures such effects cannot accidentally feed back into the
dispatch loop:

```fsharp
static member runToVoid(effect: Effect<Never>) : Effect<'b> =
    Effect.map absurd effect

static member runIgnoringInput(effect: Effect<'a>) : Effect<'b> =
    Effect(fun _ -> effect.run (fun _ -> ()))
```

The Haskell equivalent uses `Void` and `absurd` from `Data.Void`:

```haskell
voidEffect :: IO () -> Effect a
voidEffect = fmap absurd . Effect . const
```

---

## Reducer

Generally speaking a reducer is a function *S × A → S*. We decorate
this with (1) first-class effects and (2) an environment that carries
all side-effect implementations.

```haskell
-- Haskell
newtype Reducer s a r
  = Reducer { runReducer :: a -> r -> State s (Effect a) }
```

The Haskell version threads state via the `State` monad. In F# we make
state threading explicit with a plain tuple:

```fsharp
// F#
type Reducer<'s, 'a, 'r>(x: 's -> 'a -> 'r -> 's * Effect<'a>) =
    member _.run = x
```

The Swift/TCA equivalent for reference:

```swift
struct Reducer<State, Action, Environment> {
    let reduce: (inout State, Action, Environment) -> Effect<Action>
}
```

`inout State` and `(State, Effect)` are equivalent — `inout` is a
hygienic mutable borrow, not a leakable reference.

### Composing reducers

Two reducers run sequentially over shared state. The output state of
the first feeds into the second, and their effects are combined:

```haskell
instance Semigroup (Reducer s a r) where
    x <> y = Reducer $ \a r ->
        liftA2 (<>) (runReducer x a r) (runReducer y a r)
```

```fsharp
member __.Compose(x: Reducer<'s, 'a, 'r>) =
    Reducer (fun s a r ->
        let ls', le' = __.run s a r
        let rs', re' = x.run ls' a r
        (rs', le' + re'))

static member (>.>)(lx, rx) = lx.Compose(rx)
```

`>.>` lets you express a composed reducer as a pipeline:

```fsharp
let appReducer =
    Fiber.pullback counterReducer counterFiber
    >.> Fiber.pullback loggerReducer loggerFiber
```

### Empty reducer

```haskell
instance Monoid (Reducer s a r) where
    mempty = Reducer $ \_ _ -> noEffect
```

```fsharp
static member empty =
    Reducer(fun (s: 's) (_: 'a) (_: 'r) -> (s, Effect.empty))
```

`Reducer.merge` folds a list of reducers over `empty` using `>.>`,
equivalent to Haskell's `mconcat`.

---

## Store

Mutation and side-effect execution are isolated to the dispatch
function. Elm and Elmish expose a `Program` notion instead of a `Store`
precisely because they have no notion of pullbacks for reducer
composition.

```swift
// Swift / TCA
struct Store<S, A> {
    let send: (A) -> Void
    let subscribe: ((S) -> Void) -> Void
}
```

The Haskell version uses `IORef` for mutable state. F# uses a `mutable`
binding — same idea, equally contained:

```fsharp
type Store<'State, 'Action, 'Environment> =
    { getState  : unit -> 'State
      send      : 'Action -> unit
      subscribe : ('State -> unit) -> IDisposable }

let createStore reducer initialState environment =
    let mutable state = initialState
    let mutable subscribers : Map<Guid, 'State -> unit> = Map.empty

    let rec send action =
        let newState, effects = reducer.run state action environment
        state <- newState
        subscribers |> Map.iter (fun _ notify -> notify state)
        effects.run send       // ← effects re-enter through the same dispatch

    let subscribe notify =
        let key = Guid.NewGuid()
        subscribers <- Map.add key notify subscribers
        { new IDisposable with
            member _.Dispose() = subscribers <- Map.remove key subscribers }

    { getState = fun _ -> state; send = send; subscribe = subscribe }
```

The critical line is `effects.run send`. Effects feed back into the
same dispatch loop, so a reducer that returns `Effect.pure' Reset` will
automatically dispatch `Reset` on the next tick. This is the same
strategy employed by [miso](https://github.com/dmjio/miso) in Haskell:
mutation is hygienically isolated inside the function, and the effect
loop preserves the invariant that state changes go exclusively through
the reducer.

---

## Fiber and Pullback

The pullback is a lifting operation that allows a reducer with a
"smaller" domain to run over a "larger" domain. This is the largest
learning curve of the reducer architecture and the magic piece that
enables composable state sharing.

The Haskell version takes three arguments directly:

```haskell
pullback
    :: Reducer s a e
    -> Lens'  t s      -- state:  global t  ↔  local s
    -> Prism' b a      -- action: global b  ↔  local a
    -> (f -> e)        -- env:    global f  →  local e
    -> Reducer t b f
pullback r stateLens actionPrism envProj =
    Reducer $ \b e ->
        case b ^? actionPrism of
            Nothing -> noEffect
            Just a  -> do
                t <- get
                let (eff, s) = runState (runReducer r a $ envProj e) (t ^. stateLens)
                put $ t & stateLens .~ s
                pure $ review actionPrism <$> eff
```

In F# we bundle all three transformation functions into a `Fiber`
record, which makes the axes explicit and reusable across both pullback
and scope:

```fsharp
[<Struct>]
type Fiber<'t, 's, 'b, 'a, 'u, 'r> =
    { state       : Lens'<'t, 's>      // Lens'<globalState, localState>
      action      : Prism'<'b, 'a>     // Prism'<globalAction, localAction>
      environment : 'u -> 'r }         // globalEnv → localEnv
```

There are a lot of type variables:

- `'s` — Local state (e.g. `CounterState`)
- `'t` — Global state (e.g. `AppState`, which contains a `CounterState`)
- `'a` — Local action (e.g. a variant of `AppAction`)
- `'b` — Global action (e.g. `AppAction`)
- `'r` — Local environment
- `'u` — Global environment

The three axes of the pullback are:

1. **State** — extract the local state from global; embed the new local
   state back. `Lens'<'t,'s>` = `{ get: 't -> 's; set: 't -> 's -> 't }`.

2. **Action** — construct a global action from a local one; optionally
   extract a local action from a global one (fails for unrelated
   variants). `Prism'<'b,'a>` = `{ build: 'a -> 'b; match_: 'b -> Either<'b,'a> }`.

3. **Environment** — project the narrow local environment from the wider
   global one. A plain function `'u -> 'r`.

```fsharp
let pullback (x: Reducer<'s,'a,'r>) (fiber: Fiber<'t,'s,'b,'a,'u,'r>) =
    Reducer (fun t b u ->
        match preview fiber.action b with
        | None   -> (t, Effect.empty)              // unrelated action — ignore
        | Some a ->
            let s, localEff = x.run (view fiber.state t) a (fiber.environment u)
            let t' = set fiber.state t s
            let globalEff = Effect.map (review fiber.action) localEff
            (t', globalEff))
```

The effect remapping `Effect.map (review fiber.action)` is how a local
`Effect<CounterAction>` is automatically repackaged as
`Effect<AppAction>` — the counter domain never touches `AppAction`.

In Elm, Redux, and other frameworks this type of composition is not
supported. You call subsidiary update functions manually inside your
top-level reducer, introducing tight coupling between domains. `pullback`
eliminates that coupling entirely.

### Fiber.scope

`Fiber.scope` inverts the lens at the store level, giving any component
a fully-typed local `Store` that is unaware of the global type:

```fsharp
let scope (store: Store<'t,'b,'u>) (fiber: Fiber<'t,'s,'b,'a,'u,'r>) =
    { getState  = fun _      -> view fiber.state (store.getState ())
      subscribe = fun notify -> store.subscribe (fun t -> notify (view fiber.state t))
      send      = fun a      -> store.send (review fiber.action a) }
```

---

## Example: Counter

The counter domain is self-contained. It has no dependency on `AppState`
or `AppAction` and can be compiled and tested independently from the rest
of the app — the same property the Haskell README emphasises for its
`counter` example:

```fsharp
type CounterState       = { Value: int }
    with static member Zero = { Value = 0 }

type CounterAction      = | Increment of int | Decrement of int | Reset

type CounterEnvironment = { ResetValue: int }

let counterReducer : Reducer<CounterState, CounterAction, CounterEnvironment> =
    Reducer (fun state action env ->
        match action with
        | Increment n -> ({ state with Value = state.Value + n }, Effect.empty)
        | Decrement n ->
            let v = state.Value - n
            if v < 0 then (state, Effect.pure' Reset)   // guard: emit Reset
            else ({ state with Value = v }, Effect.empty)
        | Reset -> ({ state with Value = env.ResetValue }, Effect.empty))
```

The `Decrement` guard is the key pattern. Rather than going negative,
the reducer leaves state unchanged and returns `Effect.pure' Reset`.
The store re-dispatches it automatically — no external orchestration
needed, and no knowledge of `AppAction` leaks into the counter domain.

---

## Example: Logger

A second independent domain with the same guard pattern. When entries
exceed `MaxEntries` the reducer emits `Effect.pure' ClearLog`:

```fsharp
type LoggerState       = { Entries: string list }
    with static member Empty = { Entries = [] }

type LoggerAction      = | AddEntry of string | ClearLog

type LoggerEnvironment = { MaxEntries: int }

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
        | ClearLog -> ({ state with Entries = [] }, Effect.empty))
```

---

## Example: App Composition

The global types combine both domains. Neither `Counter` nor `Logger`
depends on these types:

```fsharp
type AppState       = { Counter: CounterState;  Logger: LoggerState }
type AppAction      = | CounterMsg of CounterAction | LoggerMsg of LoggerAction
type AppEnvironment = { Counter: CounterEnvironment; Logger: LoggerEnvironment }
```

Two fibers wire each domain into the global context:

```fsharp
let counterFiber
        : Fiber<AppState, CounterState, AppAction, CounterAction, AppEnvironment, CounterEnvironment> =
    { state       = { get = fun s -> s.Counter
                      set = fun s c -> { s with Counter = c } }
      action      = { build  = CounterMsg
                      match_ = fun a ->
                          match a with CounterMsg ca -> Right ca | _ -> Left a }
      environment = fun e -> e.Counter }

let loggerFiber
        : Fiber<AppState, LoggerState, AppAction, LoggerAction, AppEnvironment, LoggerEnvironment> =
    { state       = { get = fun s -> s.Logger
                      set = fun s l -> { s with Logger = l } }
      action      = { build  = LoggerMsg
                      match_ = fun a ->
                          match a with LoggerMsg la -> Right la | _ -> Left a }
      environment = fun e -> e.Logger }
```

Composing them gives the full app reducer:

```fsharp
let appReducer : Reducer<AppState, AppAction, AppEnvironment> =
    Fiber.pullback counterReducer counterFiber
    >.> Fiber.pullback loggerReducer loggerFiber
```

Every `AppAction` flows through both reducers in sequence. Each one
either handles or ignores the action independently, and effects from
either domain are automatically remapped to `AppAction` before
re-entering the store.

---

## Redux.Profunctor — Pullback via Profunctor Optics

`Redux.Profunctor` provides an alternative pullback implementation built
on the [Kinds](../README.md) higher-kinded type class system. It
decomposes `pullback` into three orthogonal coordinate changes, each
justified by a different profunctor constraint.

The [haskell-reducer](https://github.com/thomasmarsh/haskell-reducer)
source notes:

> *"More work could be done here to make reducer composition easier.
> Reducer is both a contravariant and covariant functor in `a`. These
> facts can be used to define `Applicative` and `Monad` instances for
> Reducer."*

`Redux.Profunctor` makes exactly that structure explicit. Each axis of
the pullback is encoded as a typeclass constraint, implemented in the
`Kinds` hierarchy, and then composed into a single `ppullback` function.

### The three zoom operations

#### `zoom` — widen the state coordinate (Cartesian / Strong)

A `Reducer<'s,'a,'r>` can be zoomed to a `Reducer<'t,'a,'r>` by
providing a profunctor optic for a `Lens' t s`. Under the hood,
`ReducerStatePData` implements `H2<ReducerStatePEncoding, 's, 't>` and
the encoding implements the `Cartesian` type class (the `Strong`
profunctor constraint), using `first + dimap`:

```fsharp
let zoom
        (optic: H2<ReducerStatePEncoding<'a,'r>,'s,'s>
             -> H2<ReducerStatePEncoding<'a,'r>,'t,'t>)
        (r: Reducer<'s,'a,'r>)
        : Reducer<'t,'a,'r>
```

`Cartesian` (`Strong`) is exactly the profunctor constraint that
characterises a `Lens`. The optic `lensToP` from
`Kinds.Control.Lens.Profunctor` converts a concrete `Lens` to this form.

#### `zoomAction` — widen the action coordinate (CoCartesian / Choice)

A `Reducer<'s,'a,'r>` can be zoomed to a `Reducer<'s,'b,'r>` by
providing a profunctor optic for a `Prism' b a`. `ReducerActionPData`
implements `H2<ReducerActionPEncoding, 'a, 'b>` and the encoding
implements `CoCartesian` (the `Choice` constraint), using `right_ + dimap`:

```fsharp
let zoomAction
        (optic: H2<ReducerActionPEncoding<'s,'r>,'a,'a>
             -> H2<ReducerActionPEncoding<'s,'r>,'b,'b>)
        (r: Reducer<'s,'a,'r>)
        : Reducer<'s,'b,'r>
```

`CoCartesian` (`Choice`) is exactly the constraint that characterises a
`Prism`. Non-matching actions silently return `(state, Effect.empty)`.

#### `magnify` — narrow the environment (plain contravariance)

Widening the environment requires only contravariant mapping — no
profunctor machinery:

```fsharp
let magnify (proj: 'u -> 'r) (r: Reducer<'s,'a,'r>) : Reducer<'s,'a,'u> =
    Reducer (fun s a u -> r.run s a (proj u))
```

### The table of correspondences

| Operation    | Profunctor constraint | Optic | Coordinate changed |
|--------------|-----------------------|-------|--------------------|
| `zoom`       | Cartesian (Strong)    | Lens  | State widened      |
| `zoomAction` | CoCartesian (Choice)  | Prism | Action widened     |
| `magnify`    | Contravariant (plain) | —     | Env narrowed       |

### PFiber — plain functions, no optics dependency

`Fiber` uses `Lens'` and `Prism'` from `Kinds.Control.Lens`. `PFiber`
replaces them with raw F# functions, removing the concrete optics
library dependency from the `Redux.Profunctor` layer entirely:

```fsharp
[<Struct>]
type PFiber<'t,'s,'b,'a,'u,'r> =
    { getState    : 't -> 's           // view the state slice
      setState    : 't -> 's -> 't     // update the state slice
      matchAction : 'b -> 'a option    // preview the action (None = not ours)
      embedAction : 'a -> 'b           // embed into the global action
      environment : 'u -> 'r }         // project the global environment
```

Inside `ppullback` the `H2` optic functions for `zoom` and `zoomAction`
are constructed from these raw functions using `first + dimap` and
`right_ + dimap` on the encoding instances defined in this assembly.

### `ppullback` — the composed profunctor pullback

`ppullback` performs all three coordinate changes in one call:

```fsharp
let ppullback (r: Reducer<'s,'a,'r>) (fiber: PFiber<'t,'s,'b,'a,'u,'r>)
        : Reducer<'t,'b,'u> =
    r
    |> magnify fiber.environment      // Reducer<'s,'a,'r> → Reducer<'s,'a,'u>
    |> zoom    lensOp                 //                   → Reducer<'t,'a,'u>
    |> zoomAction prismOp             //                   → Reducer<'t,'b,'u>
```

Composing two `ppullback`s with `>.>` gives the full app reducer —
identical in usage to `Fiber.pullback`, but the internal machinery is
driven by profunctor type classes rather than concrete optics:

```fsharp
let appReducer : Reducer<AppState, AppAction, AppEnvironment> =
    ppullback counterReducer counterFiber
    >.> ppullback loggerReducer loggerFiber
```

### `pscope` — a focused window onto the global store

`pscope` is the profunctor analogue of `Fiber.scope`. It creates a
`Store<'s,'a,'r>` from a global `Store<'t,'b,'u>` using a `PFiber`,
without any optics overhead:

```fsharp
let pscope (store: Store<'t,'b,'u>) (fiber: PFiber<'t,'s,'b,'a,'u,'r>)
        : Store<'s,'a,'r> =
    { getState  = fun _      -> fiber.getState (store.getState ())
      subscribe = fun notify -> store.subscribe (fun t -> notify (fiber.getState t))
      send      = fun a      -> store.send (fiber.embedAction a) }
```

A Counter UI component that receives a
`Store<CounterState, CounterAction, CounterEnvironment>` via `pscope` is
completely unaware of `AppState`, `AppAction`, or the Logger domain.

### Intermediate zoom steps

`Redux.Profunctor` also exports `zoom`, `zoomAction`, and `magnify`
individually. The `Redux.Profunctor.Demo` walkthrough demonstrates each
step in isolation before combining them:

```fsharp
// Step 2 — magnify: widen the environment
let magnifiedCounter : Reducer<CounterState, CounterAction, AppEnvironment> =
    magnify (fun (e: AppEnvironment) -> e.Counter) counterReducer

// Step 3 — zoom: widen the state (uses lensToP from Kinds.Control.Lens.Profunctor)
let counterStateLens : Lens<AppState, CounterState> =
    Lens((fun s -> s.Counter), (fun s c -> { s with Counter = c }))

let zoomedCounter : Reducer<AppState, CounterAction, AppEnvironment> =
    zoom (lensToP counterStateLens) magnifiedCounter

// Step 4 — zoomAction: widen the action
let counterActionPrism : Prism<AppAction, CounterAction> =
    Prism(CounterMsg, fun a -> match a with CounterMsg ca -> Right ca | _ -> Left a)

let actionZoomedCounter : Reducer<AppState, AppAction, AppEnvironment> =
    zoomAction (prismToP counterActionPrism) zoomedCounter

// Step 5 — ppullback does steps 2–4 in one call using PFiber
let appReducer : Reducer<AppState, AppAction, AppEnvironment> =
    ppullback counterReducer counterFiber
    >.> ppullback loggerReducer loggerFiber
```

---

## Project Layout

```
Redux/
├── Redux/                    Core library
│   ├── Effect.fs             Effect<'a> — composable deferred callbacks
│   ├── Reducer.fs            Reducer<'s,'a,'r> — pure state transition + effect
│   ├── Store.fs              Store<'s,'a,'r> — mutable runtime, dispatch loop
│   └── Fiber.fs              Fiber<…> record, Fiber.pullback, Fiber.scope
│
├── Redux.Test/               9 integration tests
│
├── Redux.Demo/               Annotated walkthrough using Fiber.pullback
│   ├── Counter.fs            Counter domain (self-contained)
│   ├── Logger.fs             Logger domain (self-contained)
│   ├── App.fs                Global types, Fibers, appReducer
│   └── Program.fs            Step-by-step narrative with live output
│
├── Redux.Profunctor/         Profunctor-based pullback
│   ├── ReducerStateP.fs      Cartesian (Strong) encoding — drives zoom
│   ├── ReducerActionP.fs     CoCartesian (Choice) encoding — drives zoomAction
│   ├── Zoom.fs               zoom, zoomAction, magnify
│   └── PFiber.fs             PFiber record, ppullback, pscope
│
├── Redux.Profunctor.Test/    19 unit tests
│
└── Redux.Profunctor.Demo/    Step-by-step profunctor walkthrough
    ├── Counter.fs             Counter domain (same as Redux.Demo)
    ├── Logger.fs              Logger domain (same as Redux.Demo)
    ├── App.fs                 Global types, PFibers, appReducer via ppullback
    └── Program.fs             Steps 1–6b: magnify → zoom → zoomAction → ppullback → pscope
```

---

## Building and Running

```bash
# Build all Redux projects
dotnet build Redux/Architecture.sln

# Run tests
dotnet test Redux/Architecture.sln

# Run the basic demo (Fiber.pullback walkthrough)
dotnet run --project Redux/Redux.Demo

# Run the profunctor demo (zoom/zoomAction/ppullback/pscope walkthrough)
dotnet run --project Redux/Redux.Profunctor.Demo
```

---

## Key Insight

The pullback is not a monolithic operation. It is the natural
composition of three independent, orthogonal coordinate changes, each
justified by a different categorical constraint:

```
zoom       ←→  Cartesian   (Strong)    — products / Lens
zoomAction ←→  CoCartesian (Choice)    — sums     / Prism
magnify    ←→  plain contravariance    — functions (Reader-like)
```

In Elm and Redux this composition is not supported at all. In the
Haskell `lens` implementation it is expressed as a single `pullback`
function. Making the decomposition explicit — as `Redux.Profunctor` does
— lets each axis be reasoned about, tested, and reused independently.
The key insight is that Lens and Prism are not arbitrary abstractions: they
are precisely the optics that characterise the two profunctor constraints
(`Cartesian` and `CoCartesian`) needed to pullback a reducer over a
product state type and a sum action type.
