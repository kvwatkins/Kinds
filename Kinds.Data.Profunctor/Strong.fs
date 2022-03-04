[<AutoOpen>]
module TypeClass.Strong

// Strong is the standard profunctor-optics name for Cartesian.
//
// The two names refer to the same concept — a profunctor that can thread
// product types (pairs) through it:
//
//   first  :: p a b -> p (a, c) (b, c)
//   second :: p a b -> p (c, a) (c, b)
//
// "Cartesian" is the categorical name (from Tambara modules);
// "Strong" is the name used in the Haskell profunctors package.
type Strong<'p when 'p :> ProFunctor<'p>> = Cartesian<'p>
