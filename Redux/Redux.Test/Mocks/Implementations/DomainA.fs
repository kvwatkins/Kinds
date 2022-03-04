module Redux.Test.Mocks.Implementations.DomainA

open Redux.Test.Mocks.Definitions.DomainA
open Redux

let domainAReductionFunction (state: DomainAState) (action: DomainAAction) (environment: DomainAEnvironment) =
    match action with
    | IncAField1 amount ->
        ({ state with
               AField1 = amount + state.AField1 },
         Effect.empty)
    | IncAField2 amount ->
        ({ state with
               AField2 = amount + state.AField2 },
         Effect.empty)

let domainAReducer = Reducer domainAReductionFunction

let domainAEnvironment =
    { PrintAField1 = (fun field1Value -> Effect(fun _ -> printfn $"DomainAField1Value %i{field1Value}"))
      PrintAField2 = (fun field2Value -> Effect(fun _ -> printfn $"DomainAField2Value %i{field2Value}")) }
