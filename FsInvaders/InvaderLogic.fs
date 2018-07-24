module InvaderLogic

open Invaders
open ScreenInvaderBlock
open GameParameters
open Game
open InvaderShooting
open InvaderMoving

type InvaderData = {
    Invaders: ScreenInvaderBlock
    InitialCount: int
    CurrentDirection: InvaderDirection
    }


let invaderCount data = NumberOfInvaders data.Invaders

let create invaders =
    {Invaders = invaders; InitialCount = NumberOfInvaders invaders; CurrentDirection = InvaderDirection.Horizontal Right }

type InvaderUpdateData = {Random: System.Random; ViewSize: Size; FrameCount: int; InvaderData: InvaderData; Shots: InvaderShots.T list}


let moveInvaders {ViewSize = viewSize; InvaderData = invaderData} =
    let nextMove = NextMove viewSize invaderData.CurrentDirection invaderData.Invaders

    {invaderData with 
                Invaders = MoveInvaders nextMove invaderData.Invaders; 
                CurrentDirection = nextMove
    }

// determine if the invaders will move in this frame and call the move function if so
let moveIfInvaderFrame ({FrameCount = frameCount; InvaderData = invaderData} as allData) =
    let currentCount = invaderData |> invaderCount
    let speed = match currentCount with
                | c when c <= 1 -> MaxSpeed
                | _ -> (StartSpeed - MaxSpeed) * (currentCount-1) / (invaderData.InitialCount-1) + MaxSpeed

    let framesPerMove = speed / FrameInterval

    match (frameCount + 1) % framesPerMove with
        | 0 -> (moveInvaders allData, 0)
        | _ -> (invaderData, frameCount + 1)






let updateInvaderShots allData = 
    let moved = InvaderShooting.MoveInvaderShots allData.Shots allData.ViewSize.Height

    match InvaderShooting.NextInvaderShot allData.Random allData.InvaderData.Invaders with
    | Some p -> InvaderShots.create p::moved
    | None -> moved


module Collision =
    open Game
    open Invader
    open ItemAndBoundingBox

    type CollisionResult = { Invaders: (Point * Point Option) list; ShotHits: ItemAndBoundingBox<Point> list; RemainingShots: ItemAndBoundingBox<Point> list}

    let private invaderBoundingBox size point = Rectangle.create point size

    let private invaderSize = Size.create InvaderSize InvaderSize

    let private playerShotSize = Size.create 2 20

    let private calcInvaderBoundingBox = invaderBoundingBox invaderSize

    let private calcShotBoundingBox s = Rectangle.create s playerShotSize

    let private hitTest invader (shotBoundingBox: ItemAndBoundingBox<_>) =
        Rectangle.intersect shotBoundingBox.BoundingBox <| calcInvaderBoundingBox invader

    let InvaderShotCollisionDetection playerShots invaders =

        let hitTestAllShots state (invader, blockPoint) =
            let shotHit shot remainingShots state = 
                {state with Invaders = (blockPoint, ItemAndBoundingBox.item shot |> Some) :: state.Invaders; 
                            ShotHits = shot::state.ShotHits; 
                            RemainingShots = remainingShots }

            let shotMiss _ state = {state with Invaders = (blockPoint, None) :: state.Invaders } : CollisionResult

            let findFirstHit (shotHasAlreadyHit, remainingShots, continuation) shot =
                match shotHasAlreadyHit with
                | false when hitTest invader shot -> (true, remainingShots, shotHit shot)
                | _ -> (shotHasAlreadyHit, shot::remainingShots, continuation)

            let (_, remainingShots, continuation) = state.RemainingShots |> List.fold findFirstHit (false, [], shotMiss)

            continuation remainingShots state

        let testHits invaders = 
            let shotBoundingBoxes = playerShots |> List.map (ItemAndBoundingBox.create calcShotBoundingBox)

            let initialState = { Invaders = []; ShotHits = []; RemainingShots = shotBoundingBoxes }

            invaders 
            |> ScreenInvaderBlock.allAliveInPosition 
            |> List.map (fun point -> ((ScreenInvaderBlock.blockToScreen invaders point), point))
            |> List.fold hitTestAllShots initialState

        testHits invaders

