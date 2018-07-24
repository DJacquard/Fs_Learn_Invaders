namespace Game

[<AutoOpen>]
module DomainTypes =
    // These types are used throughout the application and so they defined in a type-only module
    // helper functions that work with these types are defined in the Geometry module
    [<Struct>]
    type Point = {X: int; Y: int}

    [<Struct>]
    type Size = {Width: int; Height: int}

    [<Struct>]
    type Rectangle = Rectangle of Point * Size

    [<Struct>]
    type Invader<'T> = Invader of 'T

    type ItemAndBoundingBox<'T> = {Item: 'T; BoundingBox: Rectangle}

module ItemAndBoundingBox =
    let create f item = {Item = item; BoundingBox = f(item) }

    let item t = t.Item


