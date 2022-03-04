module Redux.Test.Mocks.Implementations.DomainB

open Redux.Test.Mocks.Definitions
open Redux

let domainBReductionFunction (state: DomainBState) (action: DomainBAction) (environment: DomainBEnvironment) =
    match action with
    | IncBField1 amount ->
        ({ state with
               BField1 = amount + state.BField1 },
         Effect.empty)
    | IncBField2 amount ->
        ({ state with
               BField2 = amount + state.BField2 },
         Effect.empty)

let domainBReducer = Reducer domainBReductionFunction

let domainBEnvironment =
    { PrintBField1 = (fun field1Value -> Effect(fun _ -> printfn $"DomainBField1Value %i{field1Value}"))
      PrintBField2 = (fun field2Value -> Effect(fun _ -> printfn $"DomainBField2Value %i{field2Value}")) }
