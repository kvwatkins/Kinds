[<AutoOpen>]
module TypeClass.MonadPlus

#nowarn "1189"
#nowarn "1190"

open Encoding

// MonadPlus: a Monad that is also an Alternative, satisfying stricter laws.
//
// The extra MonadPlus laws (beyond Monad + Alternative) are:
//   bind mzero f   = mzero   (left  zero)      — MonadPlus (strict / "left-zero" interpretation)
//   bind m (const mzero) = mzero   (right zero) — MonadPlus (plus ideal)
//
// There is no new abstract member: MonadPlus is a marker constraint.
// Having both Monad<'M> and Alternative<'M> (with the stricter laws) is sufficient.
//
// Defined as an interface so that concrete types can satisfy both constraints
// despite Monad<'M> being an abstract class.
type MonadPlus<'M when 'M :> Monad<'M> and 'M :> Alternative<'M>> = interface end

module MonadPlus =
    /// guard :: Alternative f => bool -> f unit
    /// Succeed with unit when the condition holds, fail otherwise.
    let guard<'F when 'F :> Applicative<'F> and 'F :> Alternative<'F>>
        (cond: bool) : H<'F, unit> =
        if cond then pure_<unit,'F> ()
        else altEmpty<unit,'F> ()
