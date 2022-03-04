module Kinds.Data.Identity

open Encoding
open TypeClass

// ─── Identity functor ─────────────────────────────────────────────────────────
//
// Identity<'a> = 'a  (a transparent wrapper)
//
// fmap f (Identity a) = Identity (f a)
//
// Useful for updating the focused value in a lens/iso:
//   over lens f s  =  runIdentity (lens.set (Identity << f) s)

type Identity<'a> = Identity of 'a interface H<IdentityEncoding,'a>
and IdentityEncoding() =
    inherit Functor<IdentityEncoding>()
    override __.fmap<'a,'b> (f: 'a -> 'b) (c: H<IdentityEncoding,'a>) =
        let (Identity a) = c :?> Identity<'a>
        Identity (f a) :> H<IdentityEncoding,'b>
    static member wrap<'a> (a: 'a) : H<IdentityEncoding,'a> = Identity a :> _
    static member unwrap<'a> (h: H<IdentityEncoding,'a>) : 'a =
        let (Identity a) = h :?> Identity<'a> in a

let runIdentity<'a> (h: H<IdentityEncoding,'a>) : 'a =
    IdentityEncoding.unwrap h
