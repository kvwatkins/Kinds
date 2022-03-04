/// Public API for profunctor optics.
///
/// Import this namespace to access the optic operations (view, over, set, review, preview)
/// and the concrete-to-profunctor converters (adapterToP, lensToP, prismToP, grateToP,
/// affineTraversalToP).
namespace Kinds.Control.Lens.Profunctor

open Kinds.Control.Lens.Profunctor.Profunctor

/// Type aliases for profunctor optics.
/// Each alias names the set of profunctor constraints that characterise the optic class.
[<AutoOpen>]
module Types =
    /// Any optic over profunctor 'p from source 's/'t to focus 'a/'b.
    type POptic<'p,'s,'t,'a,'b when 'p :> TypeClass.ProFunctor.ProFunctor<'p>> =
        Kinds.Control.Lens.Profunctor.Profunctor.POptic<'p,'s,'t,'a,'b>

    /// The "getter" profunctor — Forget<'r,'a,'b> wraps 'a -> 'r, ignoring 'b.
    type Forget<'r,'a,'b> = Kinds.Control.Lens.Profunctor.Profunctor.Forget<'r,'a,'b>

    /// The "constructor" profunctor — Tagged<'a,'b> wraps 'b, ignoring phantom 'a.
    type Tagged<'a,'b> = Kinds.Control.Lens.Profunctor.Profunctor.Tagged<'a,'b>

    /// The "iso-matcher" profunctor — Exchange<'a,'b,'s,'t> records forward + backward.
    type Exchange<'a,'b,'s,'t> = Kinds.Control.Lens.Profunctor.Profunctor.Exchange<'a,'b,'s,'t>

    /// The "prism-matcher" profunctor — Market<'a,'b,'s,'t> records embed + match.
    type Market<'a,'b,'s,'t> = Kinds.Control.Lens.Profunctor.Profunctor.Market<'a,'b,'s,'t>

    /// The "fold" profunctor — ForgetList<'r,'a,'b> wraps 'a -> 'r list, ignoring 'b.
    type ForgetList<'r,'a,'b> = Kinds.Control.Lens.Profunctor.Profunctor.ForgetList<'r,'a,'b>

/// Optic operations.
[<AutoOpen>]
module Optics =
    /// Extract the focused value from a structure.
    ///
    /// Example:
    ///   let nameLens = lensToP (Lens fst (fun (_,y) x -> (x,y)))
    ///   view nameLens ("Alice", 30)  // returns "Alice"
    let view    = Kinds.Control.Lens.Profunctor.Profunctor.view

    /// Apply a function to the focused value, rebuilding the structure.
    ///
    /// Example:
    ///   over nameLens String.length ("Alice", 30)  // returns (5, 30)
    let over    = Kinds.Control.Lens.Profunctor.Profunctor.over

    /// Replace the focused value with a constant.
    ///
    /// Example:
    ///   set nameLens "Bob" ("Alice", 30)  // returns ("Bob", 30)
    let set     = Kinds.Control.Lens.Profunctor.Profunctor.set

    /// Build a structure from a focused value (for Iso, Prism, and Grate).
    ///
    /// Example:
    ///   review (adapterToP myIso) 42   // applies the backward direction of the iso
    let review  = Kinds.Control.Lens.Profunctor.Profunctor.review

    /// Optionally extract the focused value from a concrete Prism.
    /// Returns None when the prism's branch does not match.
    ///
    /// Example:
    ///   preview justPrism (Some 42)  // returns Some 42
    ///   preview justPrism None       // returns None
    ///
    /// See also: `previewP` for the profunctor-polymorphic version.
    let preview = Kinds.Control.Lens.Profunctor.Profunctor.preview

    /// Profunctor-polymorphic preview — optionally extract via any Cocartesian optic.
    ///
    /// Example:
    ///   previewP (prismToP justPrism) (Some 42)  // returns Some 42
    ///   previewP (prismToP justPrism) None        // returns None
    let previewP = Kinds.Control.Lens.Profunctor.Profunctor.previewP

    /// Extract both components of an Iso without pattern-matching the concrete Adapter.
    ///
    /// Example:
    ///   withIso (adapterToP intStringIso) (fun to_ from -> to_ 42)  // "42"
    let withIso = Kinds.Control.Lens.Profunctor.Profunctor.withIso

    /// Extract the forward direction of an Iso.
    ///
    /// Example:
    ///   matching (adapterToP intStringIso) 42   // "42"
    let matching = Kinds.Control.Lens.Profunctor.Profunctor.matching

    /// Collect all focused values into a list.
    /// Returns a singleton for Lens, zero or one for Prism, all for Traversal.
    ///
    /// Example:
    ///   toListOf (lensToP nameLens) alice   // ["Alice"]
    ///   toListOf (prismToP justPrism) None  // []
    let toListOf = Kinds.Control.Lens.Profunctor.Profunctor.toListOf

    /// Zip two structures together via a Grate optic.
    ///
    /// Example:
    ///   zipWithGrate pairGrate (+) (1, 2) (3, 4)  // (4, 6)
    let zipWithGrate = Kinds.Control.Lens.Profunctor.Profunctor.zipWithGrate

/// Concrete-to-profunctor converters.
[<AutoOpen>]
module Converters =
    /// Lift a concrete Adapter (Iso) to a profunctor optic.
    /// Requires only ProFunctor — uses dimap directly.
    let adapterToP = Kinds.Control.Lens.Profunctor.Profunctor.adapterToP

    /// Lift a concrete Lens to a profunctor optic.
    /// Requires Cartesian p (Strong profunctor).
    let lensToP    = Kinds.Control.Lens.Profunctor.Profunctor.lensToP

    /// Lift a concrete Prism to a profunctor optic.
    /// Requires Cocartesian p (Choice profunctor).
    let prismToP   = Kinds.Control.Lens.Profunctor.Profunctor.prismToP

    /// Lift a concrete Grate to a profunctor optic.
    /// Requires Closed p.
    let grateToP   = Kinds.Control.Lens.Profunctor.Profunctor.grateToP

    /// Lift a concrete Affine (AffineTraversal) to a profunctor optic.
    /// Requires Cartesian p AND Cocartesian p.
    let affineToP  = Kinds.Control.Lens.Profunctor.Profunctor.affineTraversalToP
