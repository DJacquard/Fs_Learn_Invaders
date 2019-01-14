module InvaderLogic

open Invaders
open InvaderGrid
open ScreenInvaderBlock
open GameParameters
open InvaderMoving

type ItemAndBoundingBox<'T> = {Item: 'T; BoundingBox: Rectangle}

module ItemAndBoundingBox =
    let create f item = {Item = item; BoundingBox = f(item) }

    let item t = t.Item


type InvaderData = {
    Invaders: ScreenInvaderBlock
    InitialCount: int
    CurrentDirection: InvaderDirection
    }


let invaderCount invaders = numberOfLiveInvaders invaders.InvaderBlock

let create invaders =
    {Invaders = invaders; InitialCount = invaderCount invaders; CurrentDirection = Horizontal Movement.HorizontalDirection.Right }


module Collision =
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

            invaders.InvaderBlock 
            |> InvaderGrid.allAliveInPosition
            |> List.map (fun point -> ((ScreenInvaderBlock.blockToScreen invaders point), point))
            |> List.fold hitTestAllShots initialState

        testHits invaders

