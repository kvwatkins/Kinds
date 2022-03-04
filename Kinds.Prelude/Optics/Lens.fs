namespace Optics

[<Struct>]
type Lens<'s, 't, 'a, 'b> = { get: 's -> 'a; set: 's -> 'b -> 't }

type Lens'<'s, 'a> = Lens<'s, 's, 'a, 'a>

[<AutoOpen>]
module Lens =
    let create (get: 's -> 'a) (set: 's -> 'b -> 't) = { get = get; set = set }
    let view (l: Lens<'s, 't, 'a, 'b>) (s: 's) = l.get s
    let set (l: Lens<'s, 't, 'a, 'b>) (s: 's) (b: 'b) = l.set s b
    let over (l: Lens<'s, 't, 'a, 'b>) (s: 's) (f: 'a -> 'b) = view l s |> f |> set l s
