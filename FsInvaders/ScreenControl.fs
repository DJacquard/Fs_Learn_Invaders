﻿module ScreenControl

open System.Windows.Forms
open System.Drawing
open Game
open System
open GameLoop
open GameScreen
open Random
open PlayerLogic.PlayerData
open WinFormsTypes

type ScreenControl(keyMatrix: Ref<KeyMatrix.Matrix>) as this =
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

        member val GameScreen = GameScreen.Default() with get, set

        override _this.OnPreviewKeyDown(e: PreviewKeyDownEventArgs) =
            base.OnPreviewKeyDown(e)
            e.IsInputKey <- true

        override _this.OnPaintBackground(_) =
            ()

        override this.OnPaint(e: PaintEventArgs) =
            graphicsRef := e.Graphics

            e.Graphics.FillRectangle (Brushes.Black, System.Drawing.Rectangle(Point(0, 0), this.Size))
            
            this.Font <- new Font(new FontFamily("Arial"), 16.0f, FontStyle.Bold, GraphicsUnit.Pixel)

            let size = Size.create (this.Size.Width) (this.Size.Height)

            Renderer.draw e.Graphics size this.ClientRectangle e.Graphics this.Font random (this.GameScreen)

        member this.Tick() =

            let random = {
                NextInRange = (fun min max -> random.Next(min, max))
                NextInRangeF = (fun min max -> random.NextDouble() * (max - min) + min |> float32)
                }

            let viewSize = Size.create (this.Size.Width) (this.Size.Height)

            let isKeyPressed = KeyMatrix.isKeyPressed keyMatrix.Value
            let playerControls = {
                IsLeft = isKeyPressed System.Windows.Forms.Keys.Left
                IsRight = isKeyPressed System.Windows.Forms.Keys.Right
                IsTrigger = isKeyPressed System.Windows.Forms.Keys.Space
            }

            let gameLoopInput = {PlayerControls = playerControls; ViewSize = (Size.create (viewSize.Width) (viewSize.Height - GameParameters.hudHeight)); Random = random; Animations = animations}

            let frameData = { GameData = this.GameScreen; GameLoopInput = gameLoopInput; FrameSize = viewSize }

            this.GameScreen <- GameScreen.update frameData

            this.Invalidate()

                    
    end