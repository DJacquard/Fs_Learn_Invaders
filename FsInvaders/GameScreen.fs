module GameScreen

open GameLoop
open Particles
//open System


module FrameRate =
    type T = {
            frameCount: int
            ticksStart: int
            rate: single
        }

    let Default = { frameCount = 0; ticksStart = 0; rate = 0.0f }

    let calculate frameRate now =
        (float32 frameRate.frameCount) / float32 ((now - frameRate.ticksStart))

    let incrementFrame frameRate = {frameRate with frameCount = frameRate.frameCount + 1}

    let update frameRate now =
        if now - frameRate.ticksStart >= 2000 then
            {frameRate with ticksStart = now; rate = calculate frameRate now}
        else
            frameRate

type VariableGameData = {
    gameState: GameState.State
    explosions: ParticleCloud.ParticleCloud list
    starfields: StarField.Starfield list
    frameRate: FrameRate.T
        }

let Default() =
    {
        gameState = GameLoop.GameState.State.Default
        explosions = []
        starfields = []
        frameRate = FrameRate.Default
    }

type FrameData = {
    FrameSize: Geometry.Size
    GameData: VariableGameData
    GameLoopInput: GameLoopInput
    }


let update frameData =
    let newHits, gameState = GameLoop.run frameData.GameLoopInput frameData.GameData.gameState 

    let explosions = List.append frameData.GameData.explosions (newHits |> List.map (fun s -> Particles.Explosion.create 80 s.X s.Y 5.0 frameData.GameLoopInput.Random))

    let explosions = explosions |> List.map (fun s -> Particles.Explosion.update s 0.05f)

    let explosions = explosions |> List.filter (fun s -> (Explosion.top s).Y <= single frameData.FrameSize.Height)

    let starfields =
        if List.length frameData.GameData.starfields < 3 then
                [for i in 1 .. 3 do yield StarField.create (frameData.GameLoopInput.Random) (frameData.FrameSize) (50 - i*10) (int (2.0**float i))]
            else
                frameData.GameData.starfields |> List.map (StarField.move (frameData.GameLoopInput.Random) (frameData.FrameSize))

    {
        gameState = gameState
        explosions = explosions
        starfields = starfields
        frameRate = FrameRate.update (frameData.GameData.frameRate) (System.Environment.TickCount)
    }
