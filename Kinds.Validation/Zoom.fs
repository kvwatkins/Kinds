namespace Kinds.Validation

open Encoding

/// Optic-based zoom combinators for Validator.
///
/// zoom and zoomCase are both simply function application — the profunctor optic
/// (lensToP lens or prismToP prism) is already specialised to ValidatorEncoding<'e>
/// by the constraint on 'p, so calling it on the inner validator produces the
/// outer validator.
[<AutoOpen>]
module Zoom =

    /// Focus a field validator on a record type using a Cartesian profunctor optic.
    ///
    ///   zoom (lensToP nameLens) nameValidator
    ///   : Validator<'e, Record, Record>
    ///
    /// The optic must be specialised to ValidatorEncoding<'e>, which implements Cartesian.
    let zoom
            (optic: H2<ValidatorEncoding<'e>,'a,'a> -> H2<ValidatorEncoding<'e>,'b,'b>)
            (v: Validator<'e,'a,'a>)
            : Validator<'e,'b,'b> =
        optic v

    /// Validate one variant of a sum type using a CoCartesian profunctor optic.
    ///
    ///   zoomCase (prismToP premiumPrism) personValidator
    ///   : Validator<'e, Account, Account>
    ///
    /// If the prism does not match the input (wrong variant), the validator
    /// passes the value through as Right unchanged.
    /// If it does match, the inner validator runs on the extracted value.
    let zoomCase
            (optic: H2<ValidatorEncoding<'e>,'a,'a> -> H2<ValidatorEncoding<'e>,'b,'b>)
            (v: Validator<'e,'a,'a>)
            : Validator<'e,'b,'b> =
        optic v
