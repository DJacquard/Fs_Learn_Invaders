namespace Game

open Rectangle

open Invaders
open InvaderLogic
open InvaderShooting

module Player =
    type T = Player of int

module Level =

    type T = {
        InvaderData: InvaderData 
        PlayerX: int 
        PlayerShots: Point list 
        InvaderShots: InvaderShots.T list
        Random: System.Random
        FrameCount: int 
        PlayerHitFrameCount: int
        }
        with
            static member Default invaderData = {InvaderData = invaderData; PlayerX = 0; PlayerShots = []; InvaderShots = []; Random = System.Random(); FrameCount = 0; PlayerHitFrameCount = 0 }

  
module Logic =

    let PlayerShotCollisionDetection invaderShots playerRect =
        let shotHit =
            InvaderShots.apply (fun r -> Rectangle.intersect r playerRect) 

        invaderShots |> List.tryFind shotHit





