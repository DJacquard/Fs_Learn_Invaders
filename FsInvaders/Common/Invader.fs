[<AutoOpen>]
module Invader

// These types are used throughout the application and so they defined in a type-only module
// helper functions that work with these types are defined in the Geometry module
[<Struct>]
type Invader = Invader of bool

module Invader =
    let fromValue value = Invader value

    let create() = Invader true

    let value (Invader isAlive) = isAlive

    let apply f (Invader isAlive) = f isAlive |> Invader

    let isAlive = value




