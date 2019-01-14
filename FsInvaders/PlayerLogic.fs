module PlayerLogic

module PlayerData =
    type PlayerControls = {
        IsLeft: bool
        IsRight: bool
        IsTrigger: bool
    }

    type TriggerLatchState = TriggerLatchState of bool

    type Player = {
        TriggerLatch: TriggerLatchState
        Position: int
        Shots: Point list
    }
    with static member Create() = {TriggerLatch = false |> TriggerLatchState; Position = 0; Shots = []}


open PlayerData

module Movement =
    open Movement
    type Direction = Stationary | Moving of HorizontalDirection

    type FireWeapon = No | Yes

    open GameParameters

    let private nextMove player playerControls width =
        match (playerControls.IsLeft), (playerControls.IsRight) with
                  | true, false when player.Position - PlayerSpeed > 0 -> HorizontalDirection.Left |> Moving
                  | false, true when player.Position + PlayerSpeed < width -> HorizontalDirection.Right |> Moving
                  | _ -> Stationary

    let movePlayer player playerControls width =
        player.Position +
            match nextMove player playerControls width with
            | Moving direction -> match direction with
                                    | Left -> -PlayerSpeed
                                    | Right -> PlayerSpeed
            | _ -> 0
        |> (fun newX -> {player with Position = newX})

module Shooting =

    let updateShotPositions = 
        let moveShot = Point.moveY -10
        let removeShots shot = shot.Y > 0
        List.filter removeShots >> List.map moveShot

    let prefixPlayerShot playerX viewHeight shots =
        let newShot = Point.create (playerX + 15) (viewHeight - 40)
        newShot :: shots


    let updateShots player playerControls viewHeight = 
        let updatedShots = updateShotPositions player.Shots
        match player.TriggerLatch, (playerControls.IsTrigger) with
        | (TriggerLatchState false, true) -> (updatedShots |> prefixPlayerShot player.Position viewHeight, TriggerLatchState true)
        | (_, triggerState) -> (updatedShots, TriggerLatchState triggerState)
        |> (fun (shots, latch) -> {player with Shots = shots; TriggerLatch = latch})


module Collision =
    open Invaders
    open GameParameters

    let playerRect playerX viewHeight =
        Rectangle.create (Point.create playerX (viewHeight - PlayerHeight)) (Size.create PlayerWidth PlayerHeight)

    let checkPlayerHit playerX viewHeight invaderShots =
        let playerRect = playerRect playerX viewHeight
        let testShotHit =
            InvaderShot.apply (Rectangle.intersect playerRect)

        invaderShots |> List.exists testShotHit
