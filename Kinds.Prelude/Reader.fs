namespace Prelude

[<Struct>]
type Reader<'r, 'a> = Reader of ('r -> 'a)

[<RequireQualifiedAccess>]
module Reader =
    let inline value (x: 'a) : Reader<'r, 'a> = Reader(fun _ -> x)
    let inline apply (fn: 'r -> 'a) : Reader<'r, 'a> = Reader fn
    let run (Reader x) : 'r -> 'a = x
    let bind (f: 'a -> _) (Reader m) : Reader<'r, 'b> = Reader(fun r -> run (f (m r)) r)

    [<Struct>]
    type ReaderBuilder =
        member inline _.Return v : Reader<'r, 'a> = value v
        member inline _.Zero() : Reader<'r, 'a> = value Unchecked.defaultof<_>
        member inline _.ReturnFrom(reader: Reader<'r, 'a>) : Reader<'r, 'a> = reader
        member inline _.Bind(reader, fn) : Reader<'r, 'b> = bind fn reader

module ReaderBuilder =
    let reader = Reader.ReaderBuilder()