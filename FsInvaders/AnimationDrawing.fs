module AnimationDrawing

open Animation
open Character
open Sprite

let draw animation graphics =
    let sprites = animation.characters |> List.map (fun c -> c.sprite)
    sprites |> List.iter (fun s -> 
                match s.visible with
                | true -> s.drawFunction s.location
                | _ -> ())
