namespace Prelude

// Void is the uninhabited type from Haskell.
// A function that returns `Void` is a function that never returns.
// A function that takes `Void` as an argument can never be called.

// The becomes useful provide a callback,
// but that callback should not be used (because it has no meaningful implementation)

type Never = private Void of Never

[<AutoOpen>]
module Never =

    // absurd is a tool that is used, when a function is required as an argument (most commonly a callback),
    // but you want to enforce that it will not be called.

    let rec absurd (x: Never) : 'a =
        failwith "never called"
        absurd x