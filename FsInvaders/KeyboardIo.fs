module KeyboardIo

open System.Windows.Forms

[<Struct>]
type KeyState = KeyDown | KeyUp

let mutable Left = KeyUp;
let mutable Right = KeyUp;
let mutable Fire = KeyUp;
let mutable FireHeld = false;
let mutable Quit = KeyUp;

let private KeyPress value keyCode =
    match keyCode with
    | Keys.Left -> Left <- value
    | Keys.Right -> Right <- value
    | Keys.Space -> 
        let fire, fireHeld =
            match value, FireHeld with
            | KeyDown, false -> (KeyDown, true)
            | KeyUp, _ -> (KeyUp, false)
            | _, fireHeld -> (KeyUp, fireHeld)

        Fire <- fire
        FireHeld <- fireHeld

    | Keys.Escape -> Quit <- value
    | _ -> ()

let IsLeft() = Left = KeyDown

let IsRight() = Right = KeyDown

let IsFire() = 
    let f = Fire = KeyDown
    Fire <- KeyUp
    f

let IsQuit() = Quit = KeyDown

let KeyUp = KeyPress KeyUp

let KeyDown = KeyPress KeyDown

