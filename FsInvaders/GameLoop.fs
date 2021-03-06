﻿module GameLoop

open Game
open InvaderLogic
open GameParameters
open Invaders

open Animation

module GameState =
    open PlayerLogic.PlayerData

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

    type LevelState = WaitScreen of WaitScreen * ScreenWait | Level of LevelData

    type SubInGameState = {Level: LevelNumber; Lives: NumberOfLives;  }

    type InGameState = { LevelState: LevelState; SubState: SubInGameState }

    type State = StartScreen | InGame of InGameState
        with static member Default = StartScreen

    type LevelResult = Continue of LevelData | PlayerDeath of LevelData | Complete

type AnimationFactory = GameState.SubInGameState->Animation

type InGameAnimations = {
    Intro: AnimationFactory
    }

type GameLoopInput = {
    PlayerControls: PlayerLogic.PlayerData.PlayerControls
    ViewSize: Geometry.Size
    Random: Random.RandomSource
    Animations: InGameAnimations
}



open GameState
open Level

let runLevel levelData { PlayerControls=playerControls; ViewSize=viewSize; Random=random } =

    if levelData.PlayerHitFrameCount = 0 then
        
        let levelData = 
            levelData
            |> MoveInvaders viewSize
            |> UpdateInvaderShots (Random.nextInRange random) viewSize
            |> MovePlayer playerControls viewSize
            |> UpdatePlayerShots playerControls viewSize

        let result = InvaderLogic.Collision.InvaderShotCollisionDetection levelData.Player.Shots levelData.InvaderData.Invaders

        let (invaderHits, shotHits) = result.Invaders 
                                        |> List.filter (function (_, None) -> false | _ -> true) 
                                        |> List.map (fun (invader, shot) -> (invader, shot))
                                        |> List.unzip

        let newInvaderBlock() = 
            invaderHits |>
            List.fold (fun block point -> ScreenInvaderBlock.removeAt block point) levelData.InvaderData.Invaders

        let levelData = {levelData with InvaderData = { levelData.InvaderData with Invaders = newInvaderBlock() } }

        let newHits = shotHits |> List.choose id

        let playerHit = PlayerLogic.Collision.checkPlayerHit levelData.Player.Position viewSize.Height levelData.InvaderShots

        let newHits = 
            if playerHit then 
                Point.create (levelData.Player.Position + (PlayerWidth / 2)) (viewSize.Height - (PlayerHeight / 2))::newHits
            else
                newHits

         
        
        // if we've run out of invaders then the level is complete, otherwise create a new level data for the next frame
        let result = 
            match InvaderGrid.isEmpty levelData.InvaderData.Invaders.InvaderBlock with
            | true -> Complete
            | _ -> {levelData with 
                        Player = {levelData.Player with Shots = if playerHit then [] else result.RemainingShots |> List.map ItemAndBoundingBox.item}
                        InvaderShots = if playerHit then [] else levelData.InvaderShots
                        FrameCount = levelData.FrameCount + 1
                        PlayerHitFrameCount = if playerHit then 120 else 0
                        } |> if playerHit then PlayerDeath else Continue

        (newHits, result)
    else
        ([], {levelData with PlayerHitFrameCount = levelData.PlayerHitFrameCount - 1} |> Continue)



let createLevel() =
    let area = {Width = GameParameters.InvAreaX; Height = (GameParameters.NumberOfInvaderRows * 3 / 2) * GameParameters.InvaderSize}

    let invaders = ScreenInvaderBlock.create area.Width area.Height GameParameters.NumberOfInvaderColumns GameParameters.NumberOfInvaderRows |> InvaderLogic.create

    LevelData.Default invaders 

let runWaitScreen waitScreen wait inGameState gameLoopInput =
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
            | LevelComplete -> (LevelIntro (gameLoopInput.Animations.Intro inGameState) , ScreenWait (2*40)) |> WaitScreen
            | GameOver -> (GameOver, sw) |> WaitScreen
        | ScreenWait frame -> 
            (waitScreen, ScreenWait (frame - 1)) |> WaitScreen

let runLevelWrapper gameState gameLoopInput =
    let updateLevelState levelState = {gameState with LevelState = levelState }

    let newHits, inGameState =
        match gameState.LevelState with
        | WaitScreen (waitScreen, wait) -> ([], {gameState with LevelState = runWaitScreen waitScreen wait gameState.SubState gameLoopInput})
        | Level levelData -> 
            let newHits, levelResult = runLevel levelData gameLoopInput
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

let runStartScreen isStart state createLevel =
    match isStart with
    | true -> createLevel()
    | false -> state

let run gameLoopInput state =

    let createLevel() =
        let subState = {Level = LevelNumber 1; Lives = NumberOfLives.FromInt 3}
        {LevelState = (LevelIntro (gameLoopInput.Animations.Intro subState), ScreenWait (2*40)) |> WaitScreen; SubState = subState} |> InGame

    match state with
    | StartScreen -> ([], runStartScreen gameLoopInput.PlayerControls.IsTrigger state createLevel)
    | InGame {LevelState = WaitScreen (GameOver, ScreenWait 0)} -> ([], StartScreen)
    | InGame gameState -> runLevelWrapper gameState gameLoopInput

