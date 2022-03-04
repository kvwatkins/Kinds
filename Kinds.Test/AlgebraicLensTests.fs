module Kind.AlgebraicLensTests

open Xunit
open Encoding
open Kinds.Control.Lens.Concrete
open Kinds.Control.Lens.Profunctor.Profunctor

// ─────────────────────────────────────────────────────────────────────────────
// TEST DOMAIN
// ─────────────────────────────────────────────────────────────────────────────
//
// A labelled 1-D point: classify a float value as the label of the nearest
// training sample.  This mirrors the flower-classification example from the
// original AlgebraicOptics project, without external dependencies.

type LabeledPoint = { value: float; label: string }

/// Nearest-neighbour classifier: returns the label of the training point
/// closest to newV, paired with the new value itself.
let nearestLabel (training: LabeledPoint list) (newV: float) : LabeledPoint option =
    match training with
    | [] -> None
    | _  ->
        let nearest = training |> List.minBy (fun p -> abs (p.value - newV))
        Some { value = newV; label = nearest.label }

/// AlgebraicLens that focuses on the float value of a LabeledPoint.
let pointLens : AlgebraicLens<LabeledPoint, LabeledPoint, float, float> =
    AlgebraicLens((fun p -> p.value), nearestLabel)

let ptA = { value = 1.0; label = "A" }
let ptB = { value = 10.0; label = "B" }
let training = [ptA; ptB]


// ─────────────────────────────────────────────────────────────────────────────
// 1. CONCRETE AlgebraicLens
// ─────────────────────────────────────────────────────────────────────────────

module ConcreteTests =

    [<Fact>]
    let ``view extracts the focused float value`` () =
        Assert.Equal(1.0,  AlgebraicLens.view pointLens ptA)
        Assert.Equal(10.0, AlgebraicLens.view pointLens ptB)

    [<Fact>]
    let ``classify returns nearest label for given training set`` () =
        // 3.0 is closer to A (1.0) than B (10.0)
        Assert.Equal(Some { value = 3.0; label = "A" }, AlgebraicLens.classify pointLens training 3.0)
        // 7.0 is closer to B (10.0) than A (1.0)
        Assert.Equal(Some { value = 7.0; label = "B" }, AlgebraicLens.classify pointLens training 7.0)

    [<Fact>]
    let ``classify with empty training returns None`` () =
        Assert.Equal(None, AlgebraicLens.classify pointLens [] 5.0)

    [<Fact>]
    let ``over applies a function to the focus then classifies`` () =
        // view ptA = 1.0; f(1.0) = 9.0; 9.0 is closer to B (10.0)
        let result = AlgebraicLens.over pointLens (fun x -> x + 8.0) training ptA
        Assert.Equal(Some { value = 9.0; label = "B" }, result)

    [<Fact>]
    let ``over with identity equals classify on the current view`` () =
        let viaOver     = AlgebraicLens.over     pointLens id training ptA
        let viaClassify = AlgebraicLens.classify pointLens training (AlgebraicLens.view pointLens ptA)
        Assert.Equal(viaClassify, viaOver)

    [<Fact>]
    let ``set replaces focus with a constant then classifies`` () =
        // 6.0 is closer to B
        Assert.Equal(Some { value = 6.0; label = "B" }, AlgebraicLens.set pointLens 6.0 training)

    [<Fact>]
    let ``set with empty training returns None`` () =
        Assert.Equal(None, AlgebraicLens.set pointLens 6.0 [])

    [<Fact>]
    let ``exact match on a training point preserves that label`` () =
        // ptB.value = 10.0 — exact hit, must keep label B
        Assert.Equal(Some { value = 10.0; label = "B" }, AlgebraicLens.classify pointLens training 10.0)


// ─────────────────────────────────────────────────────────────────────────────
// 2. PROFUNCTOR-ENCODED AlgebraicLens
// ─────────────────────────────────────────────────────────────────────────────

module ProfunctorTests =

    let pOptic = algebraicLensToP pointLens

    [<Fact>]
    let ``viewAlg extracts the focused value`` () =
        Assert.Equal(1.0,  viewAlg pOptic ptA)
        Assert.Equal(10.0, viewAlg pOptic ptB)

    [<Fact>]
    let ``classifyP applies the algebra`` () =
        Assert.Equal(Some { value = 3.0; label = "A" }, classifyP pOptic training 3.0)
        Assert.Equal(Some { value = 7.0; label = "B" }, classifyP pOptic training 7.0)

    [<Fact>]
    let ``classifyP with empty training returns None`` () =
        Assert.Equal(None, classifyP pOptic [] 5.0)

    [<Fact>]
    let ``withAlgebraicLens recovers both view and classify`` () =
        withAlgebraicLens pOptic (fun view cls ->
            Assert.Equal(1.0, view ptA)
            Assert.Equal(Some { value = 7.0; label = "B" }, cls training 7.0))

    [<Fact>]
    let ``profunctor and concrete operations agree`` () =
        for pt in training do
            Assert.Equal(AlgebraicLens.view pointLens pt, viewAlg pOptic pt)
        let b = 4.5
        Assert.Equal(AlgebraicLens.classify pointLens training b, classifyP pOptic training b)

    [<Fact>]
    let ``dimap pre-processes source list passed to classify`` () =
        // Create an optic that doubles every source value before classifying.
        // The training after dimap is [ptA'; ptB'] = [{value=2; label=A}, {value=20; label=B}].
        // Classifying 15.0 against {2, 20} -> nearest is 20 -> label B.
        let scale2 (p: LabeledPoint) = { p with value = p.value * 2.0 }
        let pOptic2 =
            fun (seed: H2<AlgebraicExchangeEncoding<float,float>, float, float>) ->
                let inner = pOptic seed
                AlgebraicExchangeEncoding<float,float>().dimap scale2 id inner
        Assert.Equal(Some { value = 15.0; label = "B" }, classifyP pOptic2 training 15.0)


// ─────────────────────────────────────────────────────────────────────────────
// 3. DOMAIN EXAMPLE — nearest-neighbour flower classification
// ─────────────────────────────────────────────────────────────────────────────
//
// Re-implements the flower example from the AlgebraicOptics project without
// the FSharpPlus dependency.

module FlowerDomainTests =

    type Species = Setosa | Versicolor | Virginica

    type Flower = { species: Species; measurements: float list }

    let private euclidean (xs: float list) (ys: float list) =
        List.zip xs ys
        |> List.sumBy (fun (x, y) -> (x - y) ** 2.0)
        |> sqrt

    let flowerLens : AlgebraicLens<Flower, Flower, float list, float list> =
        AlgebraicLens(
            (fun f -> f.measurements),
            (fun training newM ->
                match training with
                | [] -> None
                | _  ->
                    let nearest = training |> List.minBy (fun f -> euclidean f.measurements newM)
                    Some { species = nearest.species; measurements = newM }))

    let versicolor = { species = Versicolor; measurements = [2.0; 3.0; 4.0; 2.0] }
    let setosa     = { species = Setosa;     measurements = [5.0; 4.0; 3.0; 2.5] }
    let flowers    = [versicolor; setosa]

    [<Fact>]
    let ``view retrieves flower measurements`` () =
        Assert.Equal<float list>([2.0; 3.0; 4.0; 2.0], AlgebraicLens.view flowerLens versicolor)

    [<Fact>]
    let ``classify assigns species of nearest flower to new measurements`` () =
        let newM = [1.9; 3.2; 3.8; 2.0]   // close to versicolor
        match AlgebraicLens.classify flowerLens flowers newM with
        | Some f ->
            Assert.Equal(Versicolor, f.species)
            Assert.Equal<float list>(newM, f.measurements)
        | None -> Assert.True(false, "expected Some")

    [<Fact>]
    let ``classify with empty training returns None`` () =
        Assert.Equal(None, AlgebraicLens.classify flowerLens [] [1.0; 2.0; 3.0; 4.0])

    [<Fact>]
    let ``over applies function to measurements then re-classifies`` () =
        // Double all measurements, then classify against the training set.
        let doubled = List.map ((*) 2.0)
        match AlgebraicLens.over flowerLens doubled flowers versicolor with
        | Some f -> Assert.Equal<float list>([4.0; 6.0; 8.0; 4.0], f.measurements)
        | None   -> Assert.True(false, "expected Some")

    [<Fact>]
    let ``profunctor viewAlg retrieves flower measurements`` () =
        let pOptic = algebraicLensToP flowerLens
        Assert.Equal<float list>([2.0; 3.0; 4.0; 2.0], viewAlg pOptic versicolor)

    [<Fact>]
    let ``profunctor classifyP assigns correct species`` () =
        let pOptic = algebraicLensToP flowerLens
        let newM   = [1.9; 3.2; 3.8; 2.0]
        match classifyP pOptic flowers newM with
        | Some f -> Assert.Equal(Versicolor, f.species)
        | None   -> Assert.True(false, "expected Some")

    [<Fact>]
    let ``withAlgebraicLens recovers both flower optic components`` () =
        let pOptic = algebraicLensToP flowerLens
        withAlgebraicLens pOptic (fun view cls ->
            Assert.Equal<float list>([2.0; 3.0; 4.0; 2.0], view versicolor)
            match cls flowers [1.9; 3.2; 3.8; 2.0] with
            | Some f -> Assert.Equal(Versicolor, f.species)
            | None   -> Assert.True(false, "expected Some"))
