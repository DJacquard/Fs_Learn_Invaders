namespace Invaders
    
open GameParameters
open Invaders

[<Struct>]
type InvaderShot = InvaderShot of Rectangle

module InvaderShot =


    let create p = Rectangle.create p <| Size.create 2 20 |> InvaderShot

    let apply f (InvaderShot shot) = f shot

    let map f = InvaderShot << (apply <| f)

    let location = apply Rectangle.location

    let moveDown spd = map (Rectangle.moveY spd)


    // choose whether or not to fire a shot and choose which invader it will come from
    let nextInvaderShot (random: System.Random) invaders =
        let lowerInvaderInEachColumn invaders =
        
            let columnToLowest (_, group) =
                group |> List.maxBy (fun {Y = y} -> y)
            let columnGrouped = invaders |> List.groupBy (fun {X = x} -> x)
            columnGrouped |> List.map columnToLowest

        let chooseInvaderToShoot invaders =
            invaders
            |> List.item (random.Next(0, List.length invaders))

        let shotStartPosition point =
            let {X = x; Y = y} = ScreenInvaderBlock.blockToScreen invaders point
            Point.create (x + InvaderSize / 2) (y + InvaderSize)

        match random.Next(50) with
        | 0 -> invaders.InvaderBlock |> InvaderGrid.allAliveInPosition |> (shotStartPosition << chooseInvaderToShoot << lowerInvaderInEachColumn) |> Some
        | _ -> None


    let moveInvaderShots shots viewHeight =
        shots
        |> List.choose (moveDown 8 >> function s when (location s).Y > viewHeight -> None | s -> Some s)
    

