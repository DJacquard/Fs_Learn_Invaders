open System.Windows.Forms
open System.Drawing
open ScreenControl
open System
open Form1
open WinFormsTypes
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false

    use Form = new Form1()

    Form.Text <- "Fs Invaders"

    Form.KeyPreview <- true
    Form.PreviewKeyDown.Add((fun e -> e.IsInputKey <- true))

    let registeredKeys = Set([Keys.Left; Keys.Right; Keys.Space])

    let keyMatrixRef = ref (KeyMatrix.Matrix.Create registeredKeys)

    let keyDown key = keyMatrixRef := KeyMatrix.pressKey keyMatrixRef.Value key
    let keyUp key = keyMatrixRef := KeyMatrix.releaseKey keyMatrixRef.Value key

    Form.KeyDown.Add((fun e -> keyDown e.KeyCode))
    Form.KeyUp.Add((fun e -> keyUp e.KeyCode))

    use ScreenControl = new ScreenControl(keyMatrixRef)

    ScreenControl.Size <- Size(GameParameters.ScreenX, GameParameters.ScreenY)
    ScreenControl.Location <- Point(10, 10)

    Form.Controls.Add(ScreenControl)

    

    let formLoadHandler e =
        Form.ClientSize <- Size(GameParameters.FormX, GameParameters.FormY)

    Form.Load.Add formLoadHandler

    let timerHandler _ =
        ()
        //ScreenControl.Tick()

    use Timer = new Timer()
    Timer.Tick.Add timerHandler
    Timer.Interval <- GameParameters.FrameInterval
    Timer.Start()


    Application.Run(Form)

    0 // return an integer exit code
