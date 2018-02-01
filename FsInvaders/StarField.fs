module StarField

open Game
open System
open System.Windows.Forms

type Starfield = Starfield of System.Drawing.Brush * int * Point list

let createStarfieldPoint (random: System.Random) viewSize y =
    Point.create (random.Next(1, viewSize.Width-1)) y

let create viewSize count brush speed =
    let random = Random()
    (brush, speed,
        {1..count} 
        |> Seq.map (fun _ -> createStarfieldPoint random viewSize (random.Next(1, viewSize.Height-1)))
        |> Seq.toList
    ) |> Starfield

let move random viewSize (Starfield (colour, speed, points)) =
    let moveOrReplacePoint p =
        match Point.moveY speed p with
        | p when p.Y > Size.height viewSize -> createStarfieldPoint random viewSize 1
        | p -> p
        
    (colour, speed,
        points |> List.map moveOrReplacePoint
    ) |> Starfield

let draw surface (Starfield (brush, _, points)) =
    points |> List.iter (fun {X = x; Y = y} -> Drawing.DrawSurface.DrawPointF surface brush x y)
    