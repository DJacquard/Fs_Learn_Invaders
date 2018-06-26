module Animation

module Sprite =
    open Game

    type DrawFun = Point -> unit

    type Sprite = {
        location: Point
        xVelocity: int
        yVelocity: int
        visible: bool
        drawFunction: DrawFun
        }

    type SpriteTransformer = Sprite->Sprite

module Character =
    open Sprite

    type KeyFrame = {
        frame: int
        action: SpriteTransformer
        }

    type Character = {
        sprite: Sprite
        actions: KeyFrame list
        }

    let applyAction sprite action = action sprite

    let moveSprite sprite = {sprite with location={X=sprite.location.X + sprite.xVelocity; Y = sprite.location.Y + sprite.yVelocity}}

    let applyFrame frame ({sprite = sprite; actions = actions} as character) =
        let actionsToApply = actions 
                             |> List.where (fun {frame = kf} -> kf = frame) 
                             |> List.map (fun {action = action} -> action)
        let transformedSprite = actionsToApply |> List.fold applyAction sprite |> moveSprite
        {character with sprite=transformedSprite}

open Character

type Animation = {
    characters: Character list
    currentFrame: int
    maxFrames: int
    }

let createAnimation maxFrames characterList = {characters = characterList; currentFrame = 0; maxFrames = maxFrames}

let incrementFrame animation =
    {animation with currentFrame = animation.currentFrame + 1}

let processFrame ({characters = characters; currentFrame = currentFrame} as animation) =
    {animation with characters = characters |> List.map (applyFrame currentFrame)}

let animate animation =
    match animation |> processFrame |> incrementFrame with
    | animation when (animation.currentFrame < animation.maxFrames) -> Some animation
    | _ -> None

    
open Sprite

let createCharacter startPos visible spriteType = {
        sprite = {location=startPos; xVelocity = 0; yVelocity = 0; visible = visible; drawFunction=spriteType }
        actions = []
        } 

let createKeyframe frame transform = {frame=frame; action=transform}

let setKeyframes character keyframes = {character with actions = keyframes |> List.map (fun (frame, action) -> {frame = frame; action = action })}

let transformVisibility visibility s = {s with visible=visibility}

let transformLocation location s = {s with location=location}

let transformXVelocity xvel s = {s with xVelocity = xvel}

let transformYVelocity yvel s = {s with yVelocity = yvel }





    
