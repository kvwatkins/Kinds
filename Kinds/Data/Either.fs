module Kind.Data.Either

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