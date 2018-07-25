namespace Game

open Rectangle

open Invaders

type LevelData = {
        InvaderData: InvaderLogic.InvaderData 
        PlayerX: int 
        PlayerShots: Point list 
        InvaderShots: InvaderShot list
        Random: System.Random
        FrameCount: int 
        PlayerHitFrameCount: int
        }
        with
            static member Default invaderData = {InvaderData = invaderData; PlayerX = 0; PlayerShots = []; InvaderShots = []; Random = System.Random(); FrameCount = 0; PlayerHitFrameCount = 0 }

  
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
        let updateInvaderShots shots viewHeight random invaders = 
            let moved = InvaderShot.moveInvaderShots shots viewHeight

            match InvaderShot.nextInvaderShot random invaders with
            | Some p -> InvaderShot.create p::moved
            | None -> moved



    open InvaderMove
    open InvaderShots

    let MoveInvaders viewSize this =
        if isInvaderMovementFrame this.FrameCount this.InvaderData then
            {this with InvaderData = moveInvaders viewSize this.InvaderData }
        else
            this

    let UpdateInvaderShots viewSize this =
        {this with InvaderShots = updateInvaderShots this.InvaderShots (viewSize.Height) this.Random this.InvaderData.Invaders }

    let MovePlayer viewSize this =
        {this with PlayerX = PlayerLogic.Movement.movePlayer this.PlayerX viewSize.Width }

    let UpdatePlayerShots viewSize this =
        {this with PlayerShots = this.PlayerShots |> PlayerLogic.Shooting.updateShots this.PlayerX viewSize.Height }
    



