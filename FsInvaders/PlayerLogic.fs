module PlayerLogic

module Movement =
    type Direction = Stay | Left | Right

    type FireWeapon = No | Yes

    open GameParameters

    let private nextMove x width =
        match KeyboardIo.IsLeft(), KeyboardIo.IsRight() with
                  | true, false when x - PlayerSpeed > 0 -> Left
                  | false, true when x + PlayerSpeed < width -> Right
                  | _ -> Stay

    let movePlayer currentX width =
        currentX +
            match nextMove currentX width with
            | Left -> -PlayerSpeed
            | Right -> PlayerSpeed
            | _ -> 0

module Shooting =
    open Game

    let CheckFire x =
        match KeyboardIo.IsFire() with 
        | true -> x + 15 |> Some 
        | false -> None

    let updateShotPositions = 
        let moveShot = Point.moveY -10

        let removeShots shot = shot.Y > 0

        List.filter removeShots >> List.map moveShot

    let prefixPlayerShot playerX viewHeight shots =
        let fire = CheckFire playerX 
        match fire with
        | Some x ->
            let newShot = Point.create x (viewHeight - 40)
            newShot :: shots
        | None -> shots

    let updateShots playerX viewHeight = updateShotPositions >> prefixPlayerShot playerX viewHeight

module Collision =
    open Game
    open Invaders
    open GameParameters

    let playerRect playerX viewHeight =
        Rectangle.create (Point.create playerX (viewHeight - PlayerHeight)) (Size.create PlayerWidth PlayerHeight)

    let checkPlayerHit playerX viewHeight invaderShots =
        let playerRect = playerRect playerX viewHeight
        let testShotHit =
            InvaderShots.apply (Rectangle.intersect playerRect)

        invaderShots |> List.exists testShotHit
