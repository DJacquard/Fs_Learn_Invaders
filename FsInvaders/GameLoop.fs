module GameLoop

open Game
open Game.Level
open InvaderLogic
open GameParameters
open Invaders

open Animation

type InGameAnimations = {
    Intro: Animation
    }

module GameState =
    [<Struct>]
    type LevelNumber = LevelNumber of int

    let valueLevelNumber (LevelNumber i) = i

    let incrementLevel (LevelNumber l) = LevelNumber (l+1)

    [<Struct>]
    type NumberOfLives = private NumberOfLives of uint8 with
        static member FromInt i = NumberOfLives (uint8 i)
        static member IsDead (NumberOfLives i) = i = 0uy

    let modifyLives (NumberOfLives l) = NumberOfLives << (fun i -> if i < 0y then failwith "number of lives cannot be negative" else uint8 i) << (+) (int8 l) << int8

    let valueLives (NumberOfLives i) = i

    type ScreenWait = ScreenWait of int
    
    type WaitScreen = LevelIntro of Animation | LevelComplete | GameOver

    type LevelState = WaitScreen of WaitScreen * ScreenWait | Level of Level.T

    type InGameState = { LevelState: LevelState; Level: LevelNumber; Lives: NumberOfLives;  }

    type State = StartScreen | InGame of InGameState
        with static member Default = StartScreen

    type LevelResult = Continue of Level.T | PlayerDeath of Level.T | Complete

open GameState
open InvaderBlock
open Invaders.Invader

let runLevel levelData viewSize hitShotsProcessor =

    if levelData.PlayerHitFrameCount = 0 then
        let invaderUpdateData = {Shots = levelData.InvaderShots; Random = levelData.Random; ViewSize = viewSize; FrameCount = levelData.FrameCount; InvaderData = levelData.InvaderData;  }

        let (invaderData, frameCount) = DoIfInvaderFrame moveInvaders invaderUpdateData

        let invaderUpdateData = {invaderUpdateData with InvaderData = invaderData; FrameCount = frameCount}

        let invaderUpdateData = {invaderUpdateData with Shots = updateInvaderShots invaderUpdateData }
                
        let newPlayerX = PlayerLogic.Movement.movePlayer levelData.PlayerX viewSize.Width

        let playerShots = levelData.PlayerShots |> PlayerLogic.Shooting.updateShots newPlayerX viewSize.Height

        let result = InvaderLogic.Collision.InvaderShotCollisionDetection playerShots invaderUpdateData.InvaderData.Invaders

        let tryUnhitInvader = function
                                | (invader, None) -> Some invader
                                | _ -> None

        // update the invaders list with the invaders that haven't been hit
        let invaders = result.Invaders |> List.choose tryUnhitInvader

        // feed the invaders that have been hit in to the explosion processor
        result.Invaders 
        |> List.choose (fun (_, shot) -> shot)
        |> hitShotsProcessor

        let playerHit = PlayerLogic.Collision.checkPlayerHit levelData.PlayerX viewSize.Height invaderUpdateData.Shots

        if playerHit then Point.create (levelData.PlayerX + (PlayerWidth / 2)) (viewSize.Height - (PlayerHeight / 2))::[] |> hitShotsProcessor
        
        // if we've run out of invaders then the level is complete, otherwise create a new level data for the next frame
        match invaders.IsEmpty with
        | true -> Complete
        | _ -> {levelData with 
                    InvaderData = {invaderUpdateData.InvaderData with Invaders = InvaderBlock.Invaders invaders};
                    PlayerX = newPlayerX;
                    PlayerShots = if playerHit then [] else result.RemainingShots |> List.map ItemAndBoundingBox.item;
                    InvaderShots = if playerHit then [] else invaderUpdateData.Shots
                    FrameCount = invaderUpdateData.FrameCount
                    PlayerHitFrameCount = if playerHit then 120 else 0
                    } |> if playerHit then PlayerDeath else Continue
    else
        {levelData with PlayerHitFrameCount = levelData.PlayerHitFrameCount - 1} |> Continue


let createLevel() =
    let area = {Width = GameParameters.InvAreaX; Height = (GameParameters.NumberOfInvaderRows * 3 / 2) * GameParameters.InvaderSize}

    let invaders = InvaderBlock.Create area.Width area.Height GameParameters.NumberOfInvaderColumns GameParameters.NumberOfInvaderRows |> InvaderLogic.create

    { Level.T.Default with InvaderData = invaders }

let runWaitScreen waitScreen wait animations =
    match wait with
    | ScreenWait 0 as sw -> 
        match waitScreen with
        | LevelIntro _ -> Level (createLevel())
        | LevelComplete -> (LevelIntro animations.Intro , ScreenWait (2*40)) |> WaitScreen
        | GameOver -> (GameOver, sw) |> WaitScreen
    | ScreenWait frame -> 
        (match waitScreen with
        | LevelIntro anim -> LevelIntro (match Animation.animate anim with Some valu -> valu)
        | x -> x
        , ScreenWait (frame - 1)) |> WaitScreen

let runLevelWrapper gameState viewSize hitShotsProcessor animationFactory =
    let updateLevelState levelState = {gameState with LevelState = levelState }

    match gameState.LevelState with
    | WaitScreen (waitScreen, wait) -> {gameState with LevelState = runWaitScreen waitScreen wait animationFactory}
    | Level levelData -> 
        match runLevel levelData viewSize hitShotsProcessor with
        | Continue newLevelData -> newLevelData |> Level |> updateLevelState
        | PlayerDeath newLevelData -> 
            match modifyLives gameState.Lives (-1) with
            | lives when NumberOfLives.IsDead lives -> { gameState with Lives = lives; LevelState = (GameOver, ScreenWait(5*40)) |> WaitScreen }
            | lives -> {gameState with LevelState = newLevelData |> Level; Lives = lives }
        | Complete -> (LevelComplete, ScreenWait (2*40)) |> WaitScreen |> updateLevelState
    |> InGame

let runStartScreen state viewSize animations =
    match KeyboardIo.IsFire() with
    | true -> {LevelState = (LevelIntro animations.Intro, ScreenWait (2*40)) |> WaitScreen; Level = LevelNumber 1; Lives = NumberOfLives.FromInt 3} |> InGame
    | false -> state

let run state viewSize hitShotsProcessor animations =
    match state with
    | StartScreen -> runStartScreen state viewSize animations
    | InGame {LevelState = WaitScreen (GameOver, ScreenWait 0)} -> StartScreen
    | InGame gameState -> runLevelWrapper gameState viewSize hitShotsProcessor animations

