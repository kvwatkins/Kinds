[<AutoOpen>]
module Pervasive.Combinators
#nowarn "40"  // recursive object in `fix` is intentional
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

let force (value: Lazy<'T>) : 'T = value.Force()

// Lazy fixed-point combinator.
//
//   fix f = let rec x = lazy (f x) in x
//
// Computes the least fixed point of f in the Lazy domain.
// Useful for defining infinite structures (streams, trees) and
// implementing CoStrong for function-like profunctors.
//
// Note: in a strict language, fix f will diverge if f forces its
// argument before returning a constructor. It is safe when f is
// "guarded" by a lazy constructor (e.g. Cons, a record, a tuple
// where only one component is forced immediately).
let fix (f: Lazy<'T> -> 'T) : Lazy<'T> =
    let rec x = lazy (f x) in x
