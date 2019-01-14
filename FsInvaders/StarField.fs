module StarField

open Game
open Random

type Starfield = Starfield of int * (Point * int) list

let createStarfieldPoint random viewSize y =
    (Point.create (nextInRange random 1 viewSize.Width-1) y, nextInRange random 0 5)

let create random viewSize count speed =
    (
        speed,
        {1..count} 
        |> Seq.map (fun _ -> createStarfieldPoint random viewSize (Random.nextInRange random 1 viewSize.Height-1))
        |> Seq.toList
    ) |> Starfield

let move random viewSize (Starfield (speed, points)) =
    let moveOrReplacePoint (p, intensity) =
        (match Point.moveY speed p with
            | p when p.Y > Size.height viewSize -> createStarfieldPoint random viewSize 1
            | p -> (p, intensity))
        
    (speed, points |> List.map moveOrReplacePoint
    ) |> Starfield


    