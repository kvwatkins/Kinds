module Redux.Profunctor.Demo.Inventory

open Redux

// ─── Types ───────────────────────────────────────────────────────────────────

type InventoryState =
    { Stock: Map<string, int> }   // item name → quantity on hand

    static member Empty           = { Stock = Map.empty }
    static member With items      = { Stock = Map.ofList items }

type InventoryAction =
    | Restock of string * int     // (name, qty to add)
    | Sell    of string * int     // (name, qty sold)

// The inventory reducer only knows about its own environment — the
// threshold below which it should automatically request a restock.
type InventoryEnvironment =
    { MinimumStockLevel: int }

// ─── Reducer ─────────────────────────────────────────────────────────────────

// Key demo feature: when a Sell brings stock *at or below* the minimum,
// the reducer emits a Restock effect.  After ppullback this effect travels
// back to the store wrapped as an AppAction — the store re-dispatches it
// automatically, driving stock back up without any external orchestration.

let inventoryReducer : Reducer<InventoryState, InventoryAction, InventoryEnvironment> =
    Reducer (fun state action env ->
        match action with
        | Restock (name, qty) ->
            let current = state.Stock |> Map.tryFind name |> Option.defaultValue 0
            ({ state with Stock = state.Stock |> Map.add name (current + qty) },
             Effect.empty)

        | Sell (name, qty) ->
            let current = state.Stock |> Map.tryFind name |> Option.defaultValue 0
            let newQty  = max 0 (current - qty)
            let effect  =
                if newQty <= env.MinimumStockLevel
                then Effect.pure' (Restock (name, 10))   // trigger auto-restock
                else Effect.empty
            ({ state with Stock = state.Stock |> Map.add name newQty }, effect))
