namespace Redux.Profunctor

open Encoding
open Redux

[<AutoOpen>]
module Zoom =

    // Zoom a Reducer's state coordinate using a profunctor optic already specialized
    // to ReducerStatePEncoding.  The optic is typically obtained by applying lensToP
    // to a concrete Lens (in a layer that IS allowed to depend on Control.Lens.Profunctor),
    // or built from raw get/set functions via first + dimap on the encoding's Cartesian instance.
    //
    //   zoom optic reducer  :  Reducer<'s,'a,'r> → Reducer<'t,'a,'r>
    let zoom
            (optic: H2<ReducerStatePEncoding<'a,'r>,'s,'s> -> H2<ReducerStatePEncoding<'a,'r>,'t,'t>)
            (r: Reducer<'s,'a,'r>)
            : Reducer<'t,'a,'r> =
        let p : H2<ReducerStatePEncoding<'a,'r>,'s,'s> = ReducerStateP r.run :> _
        let p' = optic p
        let (ReducerStateP run') = p' :?> ReducerStatePData<'a,'r,'t,'t>
        Reducer run'

    // Zoom a Reducer's action coordinate using a profunctor optic already specialized
    // to ReducerActionPEncoding.  The optic is typically obtained by applying prismToP
    // to a concrete Prism, or built from raw match/embed functions via right_ + dimap.
    //
    //   zoomAction optic reducer  :  Reducer<'s,'a,'r> → Reducer<'s,'b,'r>
    let zoomAction
            (optic: H2<ReducerActionPEncoding<'s,'r>,'a,'a> -> H2<ReducerActionPEncoding<'s,'r>,'b,'b>)
            (r: Reducer<'s,'a,'r>)
            : Reducer<'s,'b,'r> =
        let p : H2<ReducerActionPEncoding<'s,'r>,'a,'a> = ReducerActionP r.run :> _
        let p' = optic p
        let (ReducerActionP run') = p' :?> ReducerActionPData<'s,'r,'b,'b>
        Reducer run'

    // Contravariantly map the environment of a Reducer.
    // No profunctor machinery needed — environment is purely consumed.
    //
    //   magnify proj reducer  :  Reducer<'s,'a,'r> → Reducer<'s,'a,'u>
    let magnify (proj: 'u -> 'r) (r: Reducer<'s,'a,'r>) : Reducer<'s,'a,'u> =
        Reducer (fun s a u -> r.run s a (proj u))
