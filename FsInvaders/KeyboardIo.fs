module KeyboardIo

open System.Windows.Forms

let mutable Left = false;
let mutable Right = false;
let mutable Fire = false;
let mutable Quit = false;

let private KeyPress value keyCode =
    match keyCode with
    | Keys.Left -> Left <- value
    | Keys.Right -> Right <- value
    | Keys.Space -> Fire <- value
    | Keys.Escape -> Quit <- value
    | _ -> ()

let KeyUp = KeyPress false

let KeyDown = KeyPress true

let IsLeft() = Left

let IsRight() = Right

let IsFire() = 
    let f = Fire
    Fire <- false
    f

let IsQuit() = Quit

