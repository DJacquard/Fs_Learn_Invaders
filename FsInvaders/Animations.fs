module Animations

open System.Drawing
open Animation

open Drawing.DrawSurface

let testAnimation (graphicsRef: Ref<Graphics>) =

    let draw pos = 
        drawRect !graphicsRef Brushes.Red (Game.DomainTypes.Rectangle (pos, {Width=30; Height=20}))

    let character = createCharacter {X = 0; Y = 0} true draw

    createAnimation 120 [[(0, transformXVelocity 10)] |> setKeyframes character]

