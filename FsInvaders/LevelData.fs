namespace Game

open Rectangle

open Invaders

type LevelData = {
        InvaderData: InvaderLogic.InvaderData 
        Player: PlayerLogic.PlayerData.Player
        InvaderShots: InvaderShot list
        FrameCount: int 
        PlayerHitFrameCount: int
        }
        with
            static member Default invaderData = {InvaderData = invaderData; Player = PlayerLogic.PlayerData.Player.Create(); InvaderShots = []; FrameCount = 0; PlayerHitFrameCount = 0 }

  
module Level =
    open InvaderLogic

    module private InvaderMove =

        let isInvaderMovementFrame frameCount invaderData =
            let currentCount = invaderData.Invaders |> invaderCount
            let speed = match currentCount with
                        | c when c <= 1 -> GameParameters.MaxSpeed
                        | _ -> (GameParameters.StartSpeed - GameParameters.MaxSpeed) * (currentCount-1) / (invaderData.InitialCount-1) + GameParameters.MaxSpeed

            let framesPerMove = speed / GameParameters.FrameInterval

            (frameCount + 1) % framesPerMove = 0

        open InvaderMoving

        let moveInvaders viewSize invaderData =
            let invaderMove = InvaderMove(viewSize, GameParameters.InvaderMove, invaderData.Invaders)

            let nextMove = invaderMove.NextMove invaderData.CurrentDirection

            {invaderData with 
                        Invaders = invaderMove.MoveInvaders nextMove; 
                        CurrentDirection = nextMove
            }

    module private InvaderShots =
        open GameParameters

        // choose whether or not to fire a shot and choose which invader it will come from
        let nextInvaderShot random invaders =
            let lowerInvaderInEachColumn invaders =
        
                let columnToLowest (_, group) =
                    group |> List.maxBy (fun {Y = y} -> y)
                let columnGrouped = invaders |> List.groupBy (fun {X = x} -> x)
                columnGrouped |> List.map columnToLowest

            let chooseInvaderToShoot invaders =
                invaders
                |> List.item (random 0 (List.length invaders))

            let shotStartPosition point =
                let {X = x; Y = y} = ScreenInvaderBlock.blockToScreen invaders point
                Point.create (x + InvaderSize / 2) (y + InvaderSize)

            match (random 0 50) with
            | 0 -> invaders.InvaderBlock |> InvaderGrid.allAliveInPosition |> (shotStartPosition << chooseInvaderToShoot << lowerInvaderInEachColumn) |> Some
            | _ -> None

        let moveInvaderShots shots viewHeight =
            let moveDown spd = InvaderShot.map (Rectangle.moveY spd)
            shots
            |> List.choose (moveDown 8 >> function s when (InvaderShot.location s).Y > viewHeight -> None | s -> Some s)


        let updateInvaderShots shots viewHeight random invaders = 
            let moved = moveInvaderShots shots viewHeight

            match nextInvaderShot random invaders with
            | Some p -> InvaderShot.create p::moved
            | None -> moved



    open InvaderMove
    open InvaderShots

    let MoveInvaders viewSize this =
        if isInvaderMovementFrame this.FrameCount this.InvaderData then
            {this with InvaderData = moveInvaders viewSize this.InvaderData }
        else
            this

    let UpdateInvaderShots random viewSize this =
        {this with InvaderShots = updateInvaderShots this.InvaderShots (viewSize.Height) random this.InvaderData.Invaders }

    let MovePlayer playerControls viewSize this =
        {this with Player = PlayerLogic.Movement.movePlayer this.Player playerControls viewSize.Width }

    let UpdatePlayerShots playerControls viewSize this =
        let player = PlayerLogic.Shooting.updateShots this.Player playerControls viewSize.Height
        {this with Player = player}
    



