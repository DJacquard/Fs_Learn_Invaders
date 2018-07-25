module Animations

open Animation

open GameLoop.GameState

open Drawing
open System.Drawing

let testAnimation (graphicsRef: Ref<Graphics>) inGameState =
    
    let numberOfLives = inGameState.Lives |> valueLives

    let font = new Font(new FontFamily("Arial"), 16.0f, FontStyle.Bold, GraphicsUnit.Pixel)

    let drawRemainingText pos =
        graphicsRef.Value.DrawString("Remaining ships:", font, System.Drawing.Brushes.Orange, PointToSysF pos)   
        
    let remainingText = createCharacter {X = 50; Y = 100} true drawRemainingText

    let drawShip pos = 
        ItemDrawing.DrawPlayer.Draw !graphicsRef pos.X pos.Y

    let ship startVis = createCharacter {X = 0; Y = 120} startVis drawShip

    let createShipChar x =
        let startFrame = x * 25
        let frames = startFrame + 24 - x * 5
        [(startFrame, transformVisibility true); (startFrame, transformXVelocity 10); (frames, transformXVelocity 0)] |> setKeyframes (ship false)


    let shipChars = [for x in 0..numberOfLives - 1 -> createShipChar x]

    createAnimation 180 
       ( List.append [
                     [(0, transformXVelocity 0)] |> setKeyframes (remainingText)
                    ]
                    shipChars)

