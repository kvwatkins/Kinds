[<AutoOpen>]
module TypeClass.Comonad

#nowarn "1189"
#nowarn "1190"

open Encoding

// Comonad: the dual of Monad — a Functor with extraction and extension.
//
//   extract  :: w a -> a
//   extend   :: (w a -> b) -> w a -> w b
//   duplicate :: w a -> w (w a)     (derived: extend id)
//
// Laws:
//   extend extract      = id                          (left  identity)
//   extract . extend f  = f                           (right identity)
//   extend f . extend g = extend (f . extend g)       (associativity)
//
// Canonical instances:
//   (a, e)  : extract (a, _) = a;  extend f (a, e) = (f (a, e), e)   (EnvComonad)
//   Stream  : extract (x::_) = x;  extend f s = fmap f (tails s)
[<AbstractClass>]
type Comonad<'W>() =
    inherit Functor<'W>()

    abstract member extract  : H<'W,'a> -> 'a
    abstract member extend   : (H<'W,'a> -> 'b) -> H<'W,'a> -> H<'W,'b>

    // duplicate :: w a -> w (w a)   — wrap each position's context
    member this.duplicate<'a> (wa: H<'W,'a>) : H<'W, H<'W,'a>> =
        this.extend<'a, H<'W,'a>> id wa

    // fmap derived from extend and extract
    override this.fmap<'a,'b> (f: 'a -> 'b) (wa: H<'W,'a>) : H<'W,'b> =
        this.extend (fun w -> f (this.extract w)) wa

type ``:: w a -> a``<'W,'a>              = H<'W,'a> -> 'a
type ``:: (w a -> b) -> w a -> w b``<'W,'a,'b> = (H<'W,'a> -> 'b) -> H<'W,'a> -> H<'W,'b>

let extract<'a,'W when 'W :> Comonad<'W>> : ``:: w a -> a``<'W,'a> =
    getTypeSignature<'W>.extract

let extend<'a,'b,'W when 'W :> Comonad<'W>> : ``:: (w a -> b) -> w a -> w b``<'W,'a,'b> =
    getTypeSignature<'W>.extend

let duplicate<'a,'W when 'W :> Comonad<'W>> (wa: H<'W,'a>) : H<'W, H<'W,'a>> =
    getTypeSignature<'W>.duplicate<'a> wa
