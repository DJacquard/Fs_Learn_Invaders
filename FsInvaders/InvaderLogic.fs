module InvaderLogic

open Invaders
open InvaderBlock
open GameParameters
open Game

type HorizontalDirection = Left | Right
type InvaderDirection = Horizontal of HorizontalDirection | Down of HorizontalDirection

type InvaderData = {
    Invaders: Invaders
    InitialCount: int
    CurrentDirection: InvaderDirection
    }
    with 
        static member Default = { Invaders = Invaders.Default; InitialCount = 0; CurrentDirection = Horizontal Right}

let invaderCount data = NumberOfInvaders data.Invaders

let create invaders =
    {InvaderData.Default with Invaders = invaders; InitialCount = NumberOfInvaders invaders }

type InvaderUpdateData = {Random: System.Random; ViewSize: Size; FrameCount: int; InvaderData: InvaderData; Shots: InvaderShots.T list}

// determine the next invader move based on the last move and the current position
let NextMove {ViewSize = size; InvaderData = invaderData} =
    match invaderData.CurrentDirection with
    | Horizontal Left when invaderData.Invaders |> CalculateInvaderArea |> AreaAtLeftEdge -> Down Right
    | Horizontal Right when invaderData.Invaders |> CalculateInvaderArea |> AreaAtRightEdge size.Width -> Down Left 
    | Down x -> Horizontal x
    | unchanged -> unchanged


// determine if the invaders will move in this frame and call the move function if so
let DoIfInvaderFrame moveFunc ({FrameCount = frameCount; InvaderData = invaderData} as allData) =
    let currentCount = invaderData |> invaderCount
    let speed = match currentCount with
                | c when c <= 1 -> MaxSpeed
                | _ -> (StartSpeed - MaxSpeed) * (currentCount-1) / (invaderData.InitialCount-1) + MaxSpeed

    let framesPerMove = speed / FrameInterval

    match (frameCount + 1) % framesPerMove with
        | 0 -> (moveFunc allData, 0)
        | _ -> (invaderData, frameCount + 1)


// moves the invaders
let moveInvaders ({InvaderData = invaderData} as allData) =
    let nextMove = NextMove allData
            
    let MoveInvader p =
        match nextMove with 
        | Horizontal Left -> Point.create(p.X - InvaderMove) p.Y
        | Horizontal Right -> Point.create(p.X + InvaderMove) p.Y
        | Down _ -> Point.create p.X (p.Y + (InvaderMove / 2))

    let movedInvaders = InvaderBlock.Map (Invader.apply MoveInvader) invaderData.Invaders

    {invaderData with Invaders = movedInvaders ; CurrentDirection = nextMove}


// choose whether or not to fire a shot and choose which invader it will come from
let NextInvaderShot {Random = random; InvaderData = invaderData} =
    let lowerInvaderInEachColumn invaders =
        let columnToLowest (_, group) =
            group |> List.maxBy (Invader.Y)
        let columnGrouped = invaders |> List.groupBy Invader.X
        columnGrouped |> List.map columnToLowest

    let chooseInvaderToShoot invaders =
        invaders
        |> List.item (random.Next(0, List.length invaders))

    let shotStartPosition invader =
        Game.Point.create (Invader.X invader + InvaderSize / 2) (Invader.Y invader + InvaderSize)

    match random.Next(50) with
    | 0 -> invaderData.Invaders |> InvaderBlock.Apply (shotStartPosition << chooseInvaderToShoot << lowerInvaderInEachColumn) |> Some
    | _ -> None


let moveShots {Shots = shots; ViewSize = viewSize} =
    shots|> List.choose (InvaderShots.moveDown 8 >> function s when (InvaderShots.location s).Y > viewSize.Height -> None | s -> Some s)


let updateInvaderShots allData = 
    let moved = moveShots allData

    match NextInvaderShot allData with
    | Some p -> InvaderShots.create p::moved
    | None -> moved


module Collision =
    open Game
    open Invader
    open ItemAndBoundingBox

    type CollisionResult = { Invaders: (Invader * Point Option) list; ShotHits: ItemAndBoundingBox<Point> list; RemainingShots: ItemAndBoundingBox<Point> list}

    let private invaderSize = Size.create InvaderSize InvaderSize

    let private playerShotSize = Size.create 2 20

    let private calcInvaderBoundingBox = Invader.boundingBox invaderSize

    let private calcShotBoundingBox s = Rectangle.create s playerShotSize

    let private hitTest invader (shotBoundingBox: ItemAndBoundingBox<_>) =
        Rectangle.intersect shotBoundingBox.BoundingBox <| calcInvaderBoundingBox invader

    let InvaderShotCollisionDetection playerShots invaders =

        let hitTestAllShots state invader =
            let shotHit shot remainingShots state = 
                {state with Invaders = (invader, ItemAndBoundingBox.item shot |> Some) :: state.Invaders; 
                            ShotHits = shot::state.ShotHits; 
                            RemainingShots = remainingShots }

            let shotMiss _ state = {state with Invaders = (invader, None) :: state.Invaders } : CollisionResult

            let findFirstHit (shotHasAlreadyHit, remainingShots, continuation) shot =
                match shotHasAlreadyHit with
                | false when hitTest invader shot -> (true, remainingShots, shotHit shot)
                | _ -> (shotHasAlreadyHit, shot::remainingShots, continuation)

            let (_, remainingShots, continuation) = state.RemainingShots |> List.fold findFirstHit (false, [], shotMiss)

            continuation remainingShots state

        let testHits invaders = 
            let shotBoundingBoxes = playerShots |> List.map (ItemAndBoundingBox.create calcShotBoundingBox)

            let initialState = { Invaders = []; ShotHits = []; RemainingShots = shotBoundingBoxes }

            invaders |> List.fold hitTestAllShots initialState

        Apply testHits invaders

