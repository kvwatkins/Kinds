namespace Kinds.Validation

open Encoding
open TypeClass.ProFunctor
open TypeClass.CoCartesian
open Kinds.Data.Either

// ValidatorData<'e,'a,'b> wraps a function 'a -> Either<'e list, 'b>.
//
// As a ProFunctor in ('a,'b):
//   dimap f g : pre-map input via f, post-map Right output via g
//   first     : validate fst of a pair, thread snd through unchanged
//   right_    : Left passes through unchanged; Right is validated
//
// This lets lensToP and prismToP compose validators over record fields
// and sum-type variants, exactly as ReducerStatePEncoding / ReducerActionPEncoding
// compose reducers in Redux.Profunctor.

type ValidatorData<'e,'a,'b> =
    | ValidatorP of ('a -> Either<'e list, 'b>)
    interface H2<ValidatorEncoding<'e>, 'a, 'b>

and ValidatorEncoding<'e>() =
    inherit ProFunctor<ValidatorEncoding<'e>>()

    // dimap f g: pre-compose f on input, post-compose g on Right output.
    override __.dimap<'a,'b,'c,'d>
            (f: 'a -> 'b) (g: 'c -> 'd)
            (pbc: H2<ValidatorEncoding<'e>,'b,'c>) =
        let (ValidatorP run) = pbc :?> ValidatorData<'e,'b,'c>
        ValidatorP (fun a ->
            match run (f a) with
            | Left errs -> Left errs
            | Right c   -> Right (g c))
        :> H2<ValidatorEncoding<'e>,'a,'d>

    // Cartesian: thread a second component unchanged through validation.
    interface Cartesian<ValidatorEncoding<'e>> with

        // first pab :: Validator (a*c) (b*c)
        // Validates the first element; the second passes through unchanged.
        member __.first<'a,'b,'c> (pab: H2<ValidatorEncoding<'e>,'a,'b>) =
            let (ValidatorP run) = pab :?> ValidatorData<'e,'a,'b>
            ValidatorP (fun (a, c) ->
                match run a with
                | Left errs -> Left errs
                | Right b   -> Right (b, c))
            :> H2<ValidatorEncoding<'e>,'a*'c,'b*'c>

        member __.second<'a,'b,'c> (pab: H2<ValidatorEncoding<'e>,'a,'b>) =
            let (ValidatorP run) = pab :?> ValidatorData<'e,'a,'b>
            ValidatorP (fun (c, a) ->
                match run a with
                | Left errs -> Left errs
                | Right b   -> Right (c, b))
            :> H2<ValidatorEncoding<'e>,'c*'a,'c*'b>

    // CoCartesian: pass Left through, validate Right.
    interface CoCartesian<ValidatorEncoding<'e>> with

        // right_ pab :: Validator (Either c a) (Either c b)
        // Left c passes through as Right (Left c); Right a is validated.
        member __.right_<'a,'b,'c> (pab: H2<ValidatorEncoding<'e>,'a,'b>) =
            let (ValidatorP run) = pab :?> ValidatorData<'e,'a,'b>
            ValidatorP (fun e ->
                match e with
                | Left c  -> Right (Left c)
                | Right a ->
                    match run a with
                    | Left errs -> Left errs
                    | Right b   -> Right (Right b))
            :> H2<ValidatorEncoding<'e>, Either<'c,'a>, Either<'c,'b>>

        member __.left_<'a,'b,'c> (pab: H2<ValidatorEncoding<'e>,'a,'b>) =
            let (ValidatorP run) = pab :?> ValidatorData<'e,'a,'b>
            ValidatorP (fun e ->
                match e with
                | Right c -> Right (Right c)
                | Left a  ->
                    match run a with
                    | Left errs -> Left errs
                    | Right b   -> Right (Left b))
            :> H2<ValidatorEncoding<'e>, Either<'a,'c>, Either<'b,'c>>

/// A validator from 'a to 'b that may fail with a list of 'e errors.
type Validator<'e,'a,'b> = H2<ValidatorEncoding<'e>,'a,'b>

[<RequireQualifiedAccess>]
module Validator =

    /// Run a validator on an input, returning Right on success or Left errors on failure.
    let run (v: Validator<'e,'a,'b>) (a: 'a) : Either<'e list, 'b> =
        let (ValidatorP f) = v :?> ValidatorData<'e,'a,'b>
        f a

    /// A validator that always succeeds, applying f to the input.
    let succeed (f: 'a -> 'b) : Validator<'e,'a,'b> =
        ValidatorP (fun a -> Right (f a)) :> _

    /// A validator that always fails with a single error.
    let fail (e: 'e) : Validator<'e,'a,'b> =
        ValidatorP (fun _ -> Left [e]) :> _

    /// Sequential composition: validate with v1 first, then pass the result to v2.
    /// Stops at the first failure.
    let andThen (v1: Validator<'e,'a,'b>) (v2: Validator<'e,'b,'c>) : Validator<'e,'a,'c> =
        ValidatorP (fun a ->
            match run v1 a with
            | Left errs -> Left errs
            | Right b   -> run v2 b)
        :> _

    /// Parallel composition with error accumulation.
    /// Both validators run independently on the same input.
    /// If both fail, all errors are concatenated.
    let combine (v1: Validator<'e,'a,'b>) (v2: Validator<'e,'a,'c>) : Validator<'e,'a,'b * 'c> =
        ValidatorP (fun a ->
            match run v1 a, run v2 a with
            | Right b,  Right c  -> Right (b, c)
            | Left e1,  Left e2  -> Left (e1 @ e2)
            | Left e1,  Right _  -> Left e1
            | Right _,  Left e2  -> Left e2)
        :> _
