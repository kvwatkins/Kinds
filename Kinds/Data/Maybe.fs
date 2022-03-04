module rec Kind.Data.Maybe

open Encoding
open TypeClass
open Pervasive.Combinators.Abbreviation

//Data Constructors
type maybeBy<'F>() =
    inherit Functor<maybeBy<'F>>()
    override __.fmap<'a,'b> (a:'a->'b) (b:H<maybeBy<'F>,'a>) =
        match unwrap b  with
        | Some n -> Just (a n)
        | None -> Nothing
    static member wrap<'a> (x : Option<'a>): H<maybeBy<'F>, 'a> =  {wrap = x} :> _
    static member unwrap<'a> (x : H<maybeBy<'F>, 'a>): Option<'a> =  (x :?> _).wrap

and isoOfOption<'F, 'a> =
    {wrap : Option<'a>}
    interface H<maybeBy<'F>, 'a>

//Data Implementations
let Just<'F, 'a> (a: 'a) : H<maybeBy<'F>, 'a> = wrap <| Some a
[<GeneralizableValue>]
let Nothing<'F, 'a> : H<maybeBy<'F>,'a> = wrap <| None

let (|Just|Nothing|) (m:maybe<'a>) =
    let s: 'a Option = unwrap m
    match s with
    | Some m -> Just m
    | None   -> Nothing

type _sig() = inherit maybeBy<_sig>()
type Maybe = maybeBy<_sig>
type maybe<'a> = H<Maybe, 'a>