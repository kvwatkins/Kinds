namespace Prelude

[<Struct>]
type State<'s,'a> = State of ('s -> 'a * 's)

[<RequireQualifiedAccess>]
module State =
    let run state (State f) = f state
    let get _ = State (fun s -> s, s)
    let put s = State (fun _ -> ((), s))
    let ret result = State(fun state -> (result, state))
    let map f s = State(fun (state: 's) ->
        let x, state = run state s
        f x, state)
    let bind b s = State(fun state ->
        let a, s' = s |> run state
        b a |> run s')
    let (>>=) s b = bind b s

    [<Struct>]
    type StateBuilder =
        member inline _.Return x = ret x
        member inline _.Bind (f,g) = f >>= g
        member inline _.ReturnFrom f = f
        member inline _.Zero () = State(fun s -> s, ())
        member inline _.Combine (f, g) = f >>= (fun _ -> g)
        member inline _.Delay f = f ()

module StateBuilder =
    let state = State.StateBuilder()