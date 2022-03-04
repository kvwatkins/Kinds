module Kinds.Data.Const

open Encoding
open TypeClass

// ─── Const functor ────────────────────────────────────────────────────────────
//
// Const<'r,'a> = 'r  (the phantom type 'a is ignored)
//
// fmap _ (Const r) = Const r   — the function is discarded
//
// Useful for extracting the focused value in a lens/iso:
//   view lens s  =  runConst (lens.get (Const s))

type Const<'r,'a> = Const of 'r interface H<ConstEncoding<'r>,'a>
and ConstEncoding<'r>() =
    inherit Functor<ConstEncoding<'r>>()
    override __.fmap<'a,'b> _ (c: H<ConstEncoding<'r>,'a>) =
        let (Const r) = c :?> Const<'r,'a>
        Const r :> H<ConstEncoding<'r>,'b>
    static member wrap<'a> (r: 'r) : H<ConstEncoding<'r>,'a> = Const r :> _
    static member unwrap<'a> (h: H<ConstEncoding<'r>,'a>) : 'r =
        let (Const r) = h :?> Const<'r,'a> in r

let runConst<'r,'a> (h: H<ConstEncoding<'r>,'a>) : 'r =
    ConstEncoding.unwrap h
