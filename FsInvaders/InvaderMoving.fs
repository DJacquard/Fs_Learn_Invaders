module InvaderMoving

open Invaders
open ScreenInvaderBlock
open Game
open GameParameters

type HorizontalDirection = Left | Right
type InvaderDirection = Horizontal of HorizontalDirection | Down of HorizontalDirection

let NextMove viewSize currentDirection invaders =
    match currentDirection with
    | Horizontal Left when invaders |> CalculateInvaderArea |> AreaAtLeftEdge -> Down Right
    | Horizontal Right when invaders |> CalculateInvaderArea |> AreaAtRightEdge viewSize.Width -> Down Left 
    | Down x -> Horizontal x
    | unchanged -> unchanged


let MoveInvaders nextMove invaders =
    
    let movedInvaders = {
        invaders with
            position = 
                let p = invaders.position
                match nextMove with 
                | Horizontal Left -> Point.create(p.X - InvaderMove) p.Y
                | Horizontal Right -> Point.create(p.X + InvaderMove) p.Y
                | Down _ -> Point.create p.X (p.Y + (InvaderMove / 2))
        }

    movedInvaders
