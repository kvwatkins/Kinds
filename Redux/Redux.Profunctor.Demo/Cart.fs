module Redux.Profunctor.Demo.Cart

open Redux

// ─── Types ───────────────────────────────────────────────────────────────────

type Item = { Name: string; PriceCents: int }

type CartState =
    { Items:         Item list
      DiscountCents: int }

    static member Empty = { Items = []; DiscountCents = 0 }

type CartAction =
    | AddItem       of Item
    | RemoveItem    of string   // item name
    | ApplyDiscount of int      // discount in cents

// The cart reducer only knows about its own environment — how many items
// the user is allowed to add before we reject.
type CartEnvironment =
    { MaxItemsAllowed: int }

// ─── Reducer ─────────────────────────────────────────────────────────────────

let cartReducer : Reducer<CartState, CartAction, CartEnvironment> =
    Reducer (fun state action env ->
        match action with
        | AddItem item ->
            if state.Items.Length >= env.MaxItemsAllowed
            then (state, Effect.empty)   // silently reject at capacity
            else ({ state with Items = state.Items @ [item] }, Effect.empty)

        | RemoveItem name ->
            ({ state with Items = state.Items |> List.filter (fun i -> i.Name <> name) },
             Effect.empty)

        | ApplyDiscount cents ->
            ({ state with DiscountCents = cents }, Effect.empty))
