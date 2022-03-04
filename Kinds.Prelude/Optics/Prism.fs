namespace Optics

open Prelude

[<Struct>]
type Prism<'s, 't, 'a, 'b> =
    { build: 'b -> 't
      match_: 's -> Either<'t, 'a> }

type Prism'<'s, 'a> = Prism<'s, 's, 'a, 'a>

[<AutoOpen>]
module Prism =
    let preview (p: Prism'<'s, 'a>) (s: 's) = right (p.match_ s)
    let review (p: Prism'<'s, 'a>) (a: 'a) = p.build a