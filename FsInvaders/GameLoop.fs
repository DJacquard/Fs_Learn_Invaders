module GameLoop

open Game
open Game.Level
open InvaderLogic
open GameParameters
open Invaders

open Animation

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

    let valueLives (NumberOfLives i) = int i

    type ScreenWait = ScreenWait of int
    
    type WaitScreen = LevelIntro of Animation | LevelComplete | GameOver

    type LevelState = WaitScreen of WaitScreen * ScreenWait | Level of Level.T

    type SubInGameState = {Level: LevelNumber; Lives: NumberOfLives;  }

    type InGameState = { LevelState: LevelState; SubState: SubInGameState }

    type State = StartScreen | InGame of InGameState
        with static member Default = StartScreen

    type LevelResult = Continue of Level.T | PlayerDeath of Level.T | Complete

type AnimationFactory = GameState.SubInGameState->Animation

type InGameAnimations = {
    Intro: AnimationFactory
    }


open GameState
open ScreenInvaderBlock
open Invaders.Invader

let runLevel levelData viewSize =

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

        let invaderHits = result.Invaders |> List.filter (function (_, None) -> false | _ -> true) |> List.map (fun (invader, _) -> invader)

        let invadersPointsToRemove = invaderHits |> List.map (ScreenInvaderBlock.screenToBlock invaderUpdateData.InvaderData.Invaders)

        let newInvaderBlock = 
            invadersPointsToRemove |>
            List.fold (fun block point -> ScreenInvaderBlock.removeAt block point) invaderUpdateData.InvaderData.Invaders

        let newHits = 
            result.Invaders 
            |> List.choose (fun (_, shot) -> shot)

        let playerHit = PlayerLogic.Collision.checkPlayerHit levelData.PlayerX viewSize.Height invaderUpdateData.Shots

        let newHits = 
            if playerHit then 
                Point.create (levelData.PlayerX + (PlayerWidth / 2)) (viewSize.Height - (PlayerHeight / 2))::newHits
            else
                newHits

         
        
        // if we've run out of invaders then the level is complete, otherwise create a new level data for the next frame
        let result = 
            match invaders.IsEmpty with
            | true -> Complete
            | _ -> {levelData with 
                        InvaderData = {invaderUpdateData.InvaderData with Invaders = newInvaderBlock};
                        PlayerX = newPlayerX;
                        PlayerShots = if playerHit then [] else result.RemainingShots |> List.map ItemAndBoundingBox.item;
                        InvaderShots = if playerHit then [] else invaderUpdateData.Shots
                        FrameCount = invaderUpdateData.FrameCount
                        PlayerHitFrameCount = if playerHit then 120 else 0
                        } |> if playerHit then PlayerDeath else Continue

        (newHits, result)
    else
        ([], {levelData with PlayerHitFrameCount = levelData.PlayerHitFrameCount - 1} |> Continue)



let createLevel() =
    let area = {Width = GameParameters.InvAreaX; Height = (GameParameters.NumberOfInvaderRows * 3 / 2) * GameParameters.InvaderSize}

    let invaders = ScreenInvaderBlock.Create area.Width area.Height GameParameters.NumberOfInvaderColumns GameParameters.NumberOfInvaderRows |> InvaderLogic.create

    Level.T.Default invaders

let runWaitScreen waitScreen wait animations inGameState =
    match waitScreen with
    | LevelIntro anim -> 
        match Animation.animate anim with
        | Some newAnim -> (LevelIntro newAnim, wait) |> WaitScreen
        | None -> (createLevel()) |> Level
    | _ ->
        match wait with
        | ScreenWait 0 as sw -> 
            match waitScreen with
            | LevelIntro _ -> Level (createLevel())
            | LevelComplete -> (LevelIntro (animations.Intro inGameState) , ScreenWait (2*40)) |> WaitScreen
            | GameOver -> (GameOver, sw) |> WaitScreen
        | ScreenWait frame -> 
            (waitScreen, ScreenWait (frame - 1)) |> WaitScreen

let runLevelWrapper gameState viewSize animationFactory =
    let updateLevelState levelState = {gameState with LevelState = levelState }

    let newHits, inGameState =
        match gameState.LevelState with
        | WaitScreen (waitScreen, wait) -> ([], {gameState with LevelState = runWaitScreen waitScreen wait animationFactory gameState.SubState})
        | Level levelData -> 
            let newHits, levelResult = runLevel levelData viewSize
            (newHits,
                match levelResult with
                | Continue newLevelData -> newLevelData |> Level |> updateLevelState
                | PlayerDeath newLevelData -> 
                    match modifyLives gameState.SubState.Lives (-1) with
                    | lives when NumberOfLives.IsDead lives ->
                        let subState = { gameState.SubState with Lives = lives }
                        { gameState with SubState = subState; LevelState = (GameOver, ScreenWait(5*40)) |> WaitScreen }
                    | lives -> 
                        let subState = { gameState.SubState with Lives = lives }
                        {gameState with LevelState = newLevelData |> Level; SubState = subState }
                | Complete -> (LevelComplete, ScreenWait (2*40)) |> WaitScreen |> updateLevelState
            )
    
    (newHits, inGameState |> InGame)

let runStartScreen state viewSize animations =
    match KeyboardIo.IsFire() with
    | true ->
        let subState = {Level = LevelNumber 1; Lives = NumberOfLives.FromInt 3}
        {LevelState = (LevelIntro (animations.Intro subState), ScreenWait (2*40)) |> WaitScreen; SubState = subState} |> InGame
    | false -> state

let run state viewSize animations =
    match state with
    | StartScreen -> ([], runStartScreen state viewSize animations)
    | InGame {LevelState = WaitScreen (GameOver, ScreenWait 0)} -> ([], StartScreen)
    | InGame gameState -> runLevelWrapper gameState viewSize animations

