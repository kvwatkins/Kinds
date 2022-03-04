[<AutoOpen>]
module Pervasive.Combinators
// binary
let constant        x _     = x
let kite            _ x     = x
let apply           f x     = f x
let thrush          x f     = f x
let join            f x     = f x x

//ternary
let compose         f g x   = f(g(x))
let revCompose      f g x   = g(f(x))
let substitution    f g x   = f(x) (g(x))
let flip            f x y   = f y x
let chain           f g x   = f (g(x)) x
let finch           x y f   = f y x
let pairing         x y f   = f x y

//4-nry
let liftA2          f g h x = f (g x) (h x)
let on              f g x y = f (g(x)) (g(y))

module Abbreviation =
    //binary
    let I   = id
    let K   = constant
    let Ki  = kite
    let A   = apply
    let T   = thrush
    let W   = join

    //ternary
    let B   = compose
    let Q   = revCompose
    let S   = substitution
    let C   = flip
    let S_  = chain
    let F   = finch
    let V   = pairing

    //4-nry
    let S'  = liftA2
    let P   = on