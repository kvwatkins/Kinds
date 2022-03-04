namespace Kinds.Validation

/// Common primitive validators.
/// These are the building blocks to compose with zoom/zoomCase.
[<AutoOpen>]
module Rules =

    open Kinds.Data.Either

    /// Fails if the string is null or empty.
    let nonEmpty : Validator<string, string, string> =
        ValidatorP (fun s ->
            if System.String.IsNullOrEmpty s
            then Left ["Value must not be empty"]
            else Right s)
        :> _

    /// Fails if the string is shorter than n characters.
    let minLength (n: int) : Validator<string, string, string> =
        ValidatorP (fun (s: string) ->
            if s.Length < n
            then Left [sprintf "Must be at least %d characters" n]
            else Right s)
        :> _

    /// Fails if the string is longer than n characters.
    let maxLength (n: int) : Validator<string, string, string> =
        ValidatorP (fun (s: string) ->
            if s.Length > n
            then Left [sprintf "Must be at most %d characters" n]
            else Right s)
        :> _

    /// Fails if the value is outside the inclusive range [lo, hi].
    let inRange<'a when 'a : comparison> (lo: 'a) (hi: 'a) : Validator<string, 'a, 'a> =
        ValidatorP (fun x ->
            if x < lo || x > hi
            then Left [sprintf "Must be in range [%A, %A]" lo hi]
            else Right x)
        :> _

    /// Fails if the option is None ("required field").
    let notNone<'a> : Validator<string, 'a option, 'a> =
        ValidatorP (fun x ->
            match x with
            | Some v -> Right v
            | None   -> Left ["Value is required"])
        :> _

    /// Fails with msg if pred returns false.
    let satisfies (msg: string) (pred: 'a -> bool) : Validator<string, 'a, 'a> =
        ValidatorP (fun x ->
            if pred x then Right x
            else Left [msg])
        :> _
