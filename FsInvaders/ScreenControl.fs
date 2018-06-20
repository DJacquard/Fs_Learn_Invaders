module ScreenControl

open System.Windows.Forms
open System.Drawing
open Game
open System
open GameLoop


type ScreenControl() as this =
    class
        inherit Control()

        do
            let timingData, timingState = TimingLoop.InitialiseTiming()
            let timingStateRef = ref timingState
            let update _timingState = 
                this.Tick()
            Application.Idle.Add(fun _ -> TimingLoop.MessageLoop timingData timingStateRef update)
            this.DoubleBuffered <- true

        let graphicsRef = ref null : Ref<Graphics>

        let random = new Random()

        let animations = {Intro = Animations.testAnimation graphicsRef}

        member val GameScreen = GameScreen.Default with get, set

        override _this.OnPreviewKeyDown(e: PreviewKeyDownEventArgs) =
            base.OnPreviewKeyDown(e)
            e.IsInputKey <- true

        override _this.OnPaintBackground(_) =
            ()

        override this.OnPaint(e: PaintEventArgs) =
            graphicsRef := e.Graphics

            e.Graphics.FillRectangle (Brushes.Black, System.Drawing.Rectangle(Point(0, 0), this.Size))
            
            let surface = Drawing.DrawSurface.Surface (e.Graphics, this.Size)

            this.Font <- new Font(new FontFamily("Arial"), 16.0f, FontStyle.Bold, GraphicsUnit.Pixel)

            let size = Size.create (this.Size.Width) (this.Size.Height)

            GameScreen.draw surface size this.ClientRectangle e.Graphics this.Font random (this.GameScreen)

        member this.Tick() =

            let viewSize = Size.create (this.Size.Width) (this.Size.Height)

            this.GameScreen <- this.GameScreen |> GameScreen.update random viewSize animations

            this.Invalidate()

                    
    end