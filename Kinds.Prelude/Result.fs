namespace Prelude

open System

[<RequireQualifiedAccess>]
module Result =
    let ofOption error = function Some s -> Ok s | None -> Error error
    let bimap f g = function Ok r -> Ok (f r) | Error e -> Error(g e)

    type ResultBuilder() =
        member _.Return(x) = Ok x

        member _.ReturnFrom(m: Result<_, _>) = m

        member _.Bind(m, f) = Result.bind f m

        member _.Zero() = None

        member _.Combine(m, f) = Result.bind f m

        member _.Delay(f: unit -> _) = f

        member _.Run(f) = f()

        member __.TryWith(m, h) =
            try __.ReturnFrom(m)
            with e -> h e

        member __.TryFinally(m, compensation) =
            try __.ReturnFrom(m)
            finally compensation()

        member __.Using(res:#IDisposable, body) =
            __.TryFinally(body res, fun () -> match res with null -> () | dispose -> dispose.Dispose())

        member __.While(guard, f) =
            if not (guard()) then Ok () else
            do f() |> ignore
            __.While(guard, f)

        member __.For(sequence:seq<_>, body) =
            __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

        //Option Interop
        member _.Bind((m, error): Option<'t> * 'e, f) = m |> ofOption error |> Result.bind f

    let result = ResultBuilder()