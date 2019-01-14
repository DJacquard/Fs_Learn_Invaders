namespace Invaders
    
[<Struct>]
type InvaderShot = InvaderShot of Rectangle

module InvaderShot =


    let create p = Rectangle.create p <| Size.create 2 20 |> InvaderShot

    let apply f (InvaderShot shot) = f shot

    let map f = InvaderShot << (apply <| f)

    let location = apply Rectangle.location



    

    