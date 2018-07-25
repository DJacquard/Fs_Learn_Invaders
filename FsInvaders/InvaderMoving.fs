namespace Invaders

module InvaderMoving =

    type HorizontalDirection = Left | Right
    type InvaderDirection = Horizontal of HorizontalDirection | Down of HorizontalDirection

    let areInvadersAtLeftEdgeOfScreen moveDistance invaders = ScreenInvaderBlock.leftEdge invaders - moveDistance < 0

    let areInvadersAtRightEdgeOfScreen totalWidth invaders = ScreenInvaderBlock.rightEdge invaders >= totalWidth

    type InvaderMove(viewSize, moveDistance, invaders) =

        member __.NextMove currentDirection =
            match currentDirection with
            | Horizontal Left when invaders |> areInvadersAtLeftEdgeOfScreen moveDistance -> Down Right
            | Horizontal Right when invaders |>areInvadersAtRightEdgeOfScreen viewSize.Width -> Down Left 
            | Down x -> Horizontal x
            | unchanged -> unchanged


        member __.MoveInvaders nextMove =
            { invaders with
                    Position = 
                        let p = invaders.Position
                        match nextMove with 
                        | Horizontal Left -> Point.create(p.X - moveDistance) p.Y
                        | Horizontal Right -> Point.create(p.X + moveDistance) p.Y
                        | Down _ -> Point.create p.X (p.Y + (moveDistance / 2))
            }


