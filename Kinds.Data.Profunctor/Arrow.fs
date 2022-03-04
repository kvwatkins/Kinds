[<AutoOpen>]
module TypeClass.Arrow

#nowarn "1189"
#nowarn "1190"

open Encoding

// Arrow: a Category that can lift pure functions and thread product types.
//
//   arr    :: (b -> c) -> a b c           (lift a pure function into the arrow)
//
// Combined with Cartesian (first/second), this gives the full Arrow interface.
//
// Laws:
//   arr id                          = id_()
//   arr (g << f)                    = compose (arr g) (arr f)
//   first (arr f)                   = arr (fun (a,c) -> (f a, c))
//   first (compose g f)             = compose (first g) (first f)
//   compose (arr fst) (first f)     = compose f (arr fst)
//
// Canonical instance: Fn  — arr f = Fn f
type Arrow<'a when 'a :> ProFunctor<'a> and 'a :> Category<'a> and 'a :> Cartesian<'a>> =
    abstract member arr<'b,'c> : ('b -> 'c) -> H2<'a,'b,'c>

let arr<'b,'c,'a when 'a :> ProFunctor<'a> and 'a :> Category<'a> and 'a :> Cartesian<'a> and 'a :> Arrow<'a>>
    (f: 'b -> 'c) : H2<'a,'b,'c> =
    (getTypeSignature<'a> :> Arrow<'a>).arr<'b,'c> f

module Arrow =
    /// Left-to-right composition  (f >>> g  in Haskell; avoids F# >>> bit-shift operator)
    let andThen<'b,'c,'d,'a when 'a :> ProFunctor<'a> and 'a :> Category<'a> and 'a :> Cartesian<'a> and 'a :> Arrow<'a>>
        (f: H2<'a,'b,'c>) (g: H2<'a,'c,'d>) : H2<'a,'b,'d> =
        compose<'b,'c,'d,'a> g f

    /// Parallel composition  (f *** g  in Haskell)
    /// Run f on the first component and g on the second, independently.
    let fanPar<'b,'c,'b2,'c2,'a when 'a :> ProFunctor<'a> and 'a :> Category<'a> and 'a :> Cartesian<'a> and 'a :> Arrow<'a>>
        (f: H2<'a,'b,'c>) (g: H2<'a,'b2,'c2>) : H2<'a,'b*'b2,'c*'c2> =
        let f' = (getTypeSignature<'a> :> Cartesian<'a>).first<'b,'c,'b2>  f
        let g' = (getTypeSignature<'a> :> Cartesian<'a>).second<'b2,'c2,'c> g
        compose<'b*'b2,'c*'b2,'c*'c2,'a> g' f'

    /// Fan-out  (f &&& g  in Haskell; avoids F# &&& bitwise-AND operator)
    /// Apply both f and g to the same input and return a pair.
    let fanOut<'b,'c,'d,'a when 'a :> ProFunctor<'a> and 'a :> Category<'a> and 'a :> Cartesian<'a> and 'a :> Arrow<'a>>
        (f: H2<'a,'b,'c>) (g: H2<'a,'b,'d>) : H2<'a,'b,'c*'d> =
        let dup  = arr<'b,'b*'b,'a> (fun x -> (x, x))
        let both = fanPar<'b,'c,'b,'d,'a> f g
        compose<'b,'b*'b,'c*'d,'a> both dup
