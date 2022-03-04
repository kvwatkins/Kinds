namespace Redux

open System

type Store<'State, 'Action, 'Environment> =
    { getState: unit -> 'State
      send: 'Action -> unit
      subscribe: ('State -> unit) -> IDisposable }

[<AutoOpen>]
module Store =
    open Reducer

    //Todo refactor to computation builder
    let createStore (reducer: Reducer<'State, 'Action, 'Environment>) initialState environment =
        let mutable state = initialState
        let mutable (subscribers: Map<Guid, 'State -> unit>) = Map.empty

        let rec send action =
            let newState, effects = (reducer.run state action environment)

            state <- newState

            subscribers
            |> Map.iter (fun _ notify -> notify state)

            effects.run send

        let subscribe notify =
            let key = Guid.NewGuid()
            subscribers <- Map.add key notify subscribers

            { new IDisposable with
                member x.Dispose() =
                    subscribers <- Map.remove key subscribers }

        let newStore: Store<'State, 'Action, 'Environment> =
            { getState = (fun _ -> state)
              send = send
              subscribe = subscribe }

        newStore