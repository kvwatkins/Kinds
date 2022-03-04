namespace Kinds.Prelude

[<AutoOpen>]
module Either =
    type Either<'a, 'b> = Choice<'b, 'a>
    let Right x : Either<'a, 'b> = Choice1Of2 x
    let Left x : Either<'a, 'b> = Choice2Of2 x
    let (|Right|Left|) = function | Choice1Of2 x -> Right x | Choice2Of2 x -> Left x
    let either f g = function | Left x -> f x | Right y -> g y

    [<RequireQualifiedAccess>]
    module Either =
        let bimap (f: 'a -> 'a0) (g: 'b -> 'b0) (e: Either<'a, 'b>) = match e with | Left a -> Left(f a) | Right b -> Right(g b)
        let map (f: 'b -> 'a0) (e: Either<'a, 'b>) = bimap id f e
        let mapLeft (f: 'a -> 'a0) (e: Either<'a, 'b>) = bimap f id e
        let bind (f: 'b -> Either<'a, 'a0>) (e: Either<'a, 'b>) : Either<'a, 'a0> = match e with | Left a -> Left a | Right b -> f b
        let right (e: Either<'a, 'b>) = match e with | Left _ -> None | Right b -> Some b
        let left (e: Either<'a, 'b>) = match e with | Left a -> Some a | Right _ -> None
        let fold (l: 'a -> 'a0) (r: 'b -> 'a0) (e: Either<'a, 'b>) = match e with | Left a -> l a | Right b -> r b