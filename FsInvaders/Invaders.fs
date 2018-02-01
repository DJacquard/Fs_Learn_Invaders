module Invaders

open Game

module Invader =
    let create p = Invader p

    let value (Invader invader) = invader

    let location = value

    let apply f (Invader invader) = f invader |> Invader

    let boundingBox size invader = Rectangle.create (invader |> value) size 

    let X i = (value i).X

    let Y i = (value i).Y

    let compareColumn i1 i2 = X i1 - X i2

    
open GameParameters

module InvaderBlock =

    type Invaders = Invaders of Invader list
        with static member Default = Invaders []

    let Apply f (Invaders invaders) =
        f invaders

    let IsEmpty =
        Apply <| List.isEmpty

    let NumberOfInvaders =
        Apply <| List.length

    let FindEdge edgeFunc start =
        Apply <| List.fold (fun state -> edgeFunc state << Invader.X) start

    let LeftEdge = FindEdge min System.Int32.MaxValue

    let RightEdge invaders = InvaderSize + FindEdge max System.Int32.MinValue invaders

    let BottomEdge =
        Apply <| List.fold (fun state -> max state << Invader.Y) System.Int32.MinValue 

    let TopEdge =
        Apply <| List.fold (fun state -> min state << Invader.Y) System.Int32.MaxValue 

    let CreateEmpty() = Invaders []

    let createPointAtY y x = Point.create x y |> Invader.create

    let Create width height columns rows =
        let xStep = (*) (width / columns)
        let yStep = (*) (height / rows)

        let placeRow y =
            {0..columns-1} |> Seq.map (xStep >> createPointAtY y) 

        {0..rows-1} |> Seq.collect (yStep >> placeRow) |> Seq.toList
            |> Invaders

    let Iterate func =
        Apply <| List.iter func

    let Map func = (Apply <| List.map func) >> Invaders

    let CalculateInvaderArea invaders =
        let x = LeftEdge invaders
        let y = TopEdge invaders
        let height = BottomEdge invaders - y
        let width = RightEdge invaders - x
        Size.create width height |> (Point.create x y |> Rectangle.create)

    let AreaAtLeftEdge area = (area |> Rectangle.leftEdge) - InvaderMove < 0

    let AreaAtRightEdge totalWidth area = (area |> Rectangle.rightEdge) >= totalWidth



module InvaderShots =
    [<Struct>]
    type T = InvaderShot of Rectangle

    let create p = Rectangle.create p <| Size.create 2 20 |> InvaderShot

    let apply f (InvaderShot shot) = f shot

    let map f = InvaderShot << (apply <| f)

    let location = apply Rectangle.location

    let moveDown spd = map (Rectangle.moveY spd)

