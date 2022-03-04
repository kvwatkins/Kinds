module Encoding

open System
open System.Reflection

type H<'K, 'T> = interface end
type H2<'K, 'T1, 'T2> = H<H<'K, 'T1>, 'T2>
type H3<'K, 'T1, 'T2, 'T3> = H<H<H<'K, 'T1>, 'T2>, 'T3>

[<GeneralizableValue>]
let getTypeSignature<'a> =
    let aType = typeof<'a>
    let defaultTypeInstance = Array.zeroCreate<Type> 0

    let constructor =
        aType.GetConstructor(
            (BindingFlags.Instance ||| BindingFlags.Public),
            null,
            CallingConventions.HasThis,
            defaultTypeInstance,
            null
        )
    let object = constructor.Invoke([||])
    object :?> 'a

let inline wrap<'o, ^f, 'a when ^f: (static member wrap : 'o -> H<'f, 'a>)> (o: 'o) : H< ^f, 'a > =
    (^f: (static member wrap : 'o -> H<'f, 'a>) o)

let inline unwrap<'o, ^f, 'a when ^f: (static member unwrap : H<'f, 'a> -> 'o)> (f: H< ^f, 'a >) : 'o =
    (^f: (static member unwrap : H<'f, 'a> -> 'o) f)