module Kinds.Data.Either

open Encoding
open TypeClass

type either<'a,'b> = H2<EitherBifunctorEncoding,'a,'b>
and EitherBifunctorEncoding() =
        interface BiFunctor<EitherBifunctorEncoding> with
            member _.bimap(a: 'a -> 'b)(b: 'c -> 'd)(c: H2<EitherBifunctorEncoding,'a,'c>) : H2<EitherBifunctorEncoding,'b,'d> =
                match c :?> eitherData<'a,'c> with
                |  Left l  -> Left (a l) :> _
                |  Right r -> Right (b r) :> _

and eitherData<'a,'b> =
    | Left of 'a
    | Right of 'b
    interface either<'a,'b>

let Left<'a,'b> (e: 'a) : either<'a,'b> = Left e :> _
let Right<'a,'b> (a: 'b ) : either<'a,'b> = Right a :> _
let (|Left|Right|) (m: either<'e, 'a>) =
    match m :?> eitherData<'e, 'a> with
    | Left l  -> Left l
    | Right r -> Right r

// Alias matching the conventional capitalisation used throughout the library.
type Either<'a,'b> = either<'a,'b>

[<RequireQualifiedAccess>]
module Either =
    let fold (l: 'a -> 'c) (r: 'b -> 'c) (e: either<'a,'b>) : 'c =
        match e with Left a -> l a | Right b -> r b

    let right (e: either<'a,'b>) : 'b option =
        match e with Left _ -> None | Right b -> Some b

    let left (e: either<'a,'b>) : 'a option =
        match e with Left a -> Some a | Right _ -> None

    let bimap (f: 'a -> 'c) (g: 'b -> 'd) (e: either<'a,'b>) : either<'c,'d> =
        match e with Left a -> Left (f a) | Right b -> Right (g b)

    let mapLeft (f: 'a -> 'c) (e: either<'a,'b>) : either<'c,'b> =
        match e with Left a -> Left (f a) | Right b -> Right b

    let mapRight (f: 'b -> 'c) (e: either<'a,'b>) : either<'a,'c> =
        match e with Left a -> Left a | Right b -> Right (f b)

    let bind (f: 'b -> either<'a,'c>) (e: either<'a,'b>) : either<'a,'c> =
        match e with Left a -> Left a | Right b -> f b
