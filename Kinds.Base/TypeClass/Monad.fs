[<AutoOpen>]
module TypeClass.Monad

#nowarn "1189"
#nowarn "1190"

open Encoding

// Monad: an Applicative with sequenced computation.
//
//   bind :: m a -> (a -> m b) -> m b
//
// Laws:
//   bind (pure_ a) f   = f a                         (left  identity)
//   bind m pure_       = m                            (right identity)
//   bind (bind m f) g  = bind m (fun x -> bind (f x) g)  (associativity)
//
// ap is derived: ap mf mx = bind mf (fun f -> bind mx (fun x -> pure_ (f x)))
[<AbstractClass>]
type Monad<'M>() =
    inherit Applicative<'M>()

    abstract member bind : H<'M,'a> -> ('a -> H<'M,'b>) -> H<'M,'b>

    // ap derived from bind and pure_; concrete instances may override for efficiency.
    override this.ap<'a,'b> (mf: H<'M,'a->'b>) (mx: H<'M,'a>) : H<'M,'b> =
        this.bind mf (fun (f: 'a -> 'b) -> this.bind mx (fun x -> this.pure_ (f x)))

type ``:: m a -> (a -> m b) -> m b``<'M,'a,'b> = H<'M,'a> -> ('a -> H<'M,'b>) -> H<'M,'b>

let bind<'a,'b,'M when 'M :> Monad<'M>> : ``:: m a -> (a -> m b) -> m b``<'M,'a,'b> =
    getTypeSignature<'M>.bind

module Operators =
    let inline (>>=) m f = bind m f
