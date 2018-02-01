module ScreenControl

open System.Windows.Forms
open System.Drawing
open Game
open ItemDrawing
open Invaders
open Logic
open GameParameters
open System
open Particles
open GameLoop
open GameLoop.GameState

type ScreenControl() as this =
    class
        inherit Control()

        do this.DoubleBuffered <- true

        let mutable frameCount = 0

        let mutable ticksStart = Environment.TickCount

        let mutable frameRate = 0.0f

        let mutable starFields = []

        let graphicsRef = ref null : Ref<Graphics>

        let random = new Random()

        let explosions = ResizeArray<ParticleCloud.ParticleCloud>()

        let animations = {Intro = Animations.testAnimation graphicsRef}

        member val GameState = GameLoop.GameState.State.Default with get, set

        override _this.OnPreviewKeyDown(e: PreviewKeyDownEventArgs) =
            base.OnPreviewKeyDown(e)
            e.IsInputKey <- true

        override _this.OnPaintBackground(_) =
            ()

        override this.OnPaint(e: PaintEventArgs) =
            graphicsRef := e.Graphics

            frameCount <- frameCount + 1000
            let now = Environment.TickCount
            if now - ticksStart >= 2000 then
                frameRate <- (float32 frameCount) / float32 ((now - ticksStart))
                ticksStart <- now
                frameCount <- 0

            e.Graphics.FillRectangle (Brushes.Black, System.Drawing.Rectangle(Point(0, 0), this.Size))
            e.Graphics.DrawString(frameRate.ToString(), this.Font, Brushes.White, PointF(0.0f,0.0f))
            let surface = Drawing.DrawSurface.Surface (e.Graphics, this.Size)

            this.Font <- new Font(new FontFamily("Arial"), 16.0f, FontStyle.Bold, GraphicsUnit.Pixel)

            let toRectangleF r = RectangleF.op_Implicit(r)

            let drawHud (LevelNumber level) lives =
                e.Graphics.DrawLine(Pens.Orange, Point(0, hudHeight - 1), Point(this.Width, hudHeight - 1))
                use sf = new StringFormat()
                sf.LineAlignment <- StringAlignment.Far
                sf.Alignment <- StringAlignment.Near
                e.Graphics.DrawString(sprintf "Level %d" level, this.Font, Brushes.Orange, RectangleF(0.0f, 0.0f, float32 this.Width, float32 hudHeight - 1.0f), sf)

                sf.Alignment <- StringAlignment.Far
                e.Graphics.DrawString(sprintf "Lives %d" (valueLives lives), this.Font, Brushes.Orange, RectangleF(0.0f, 0.0f, float32 this.Width, float32 hudHeight - 1.0f), sf)

            let drawStringCentre brush s = 
                use sf = new StringFormat()
                sf.LineAlignment <- StringAlignment.Center
                sf.Alignment <- StringAlignment.Center
                e.Graphics.DrawString(s, this.Font, brush, toRectangleF this.ClientRectangle, sf)

            let drawAnimation animation =
                AnimationDrawing.draw animation

            let drawWaitScreen screenType gameState =
                match screenType with
                | LevelIntro animation -> //drawStringCentre Brushes.Orange (sprintf "Get ready\r\nLevel: %d\r\nRemaining lives: %d" (valueLevelNumber gameState.Level) (valueLives gameState.Lives))
                    drawAnimation animation e.Graphics
                | LevelComplete -> drawStringCentre Brushes.Yellow "Level complete"
                | GameOver -> drawStringCentre Brushes.Red "Game Over"

            let drawLevel ({LevelState = levelState; SubState = subState} as gameState) =
                drawHud subState.Level subState.Lives
                starFields |> List.iter (StarField.draw surface)
                match levelState with
                | WaitScreen (screenType,_) -> drawWaitScreen screenType gameState
                | Level levelData ->
                    e.Graphics.TranslateTransform(0.0f, float32 hudHeight)            
                    DrawInvaders.DrawInvaders e.Graphics levelData.InvaderData.Invaders
                    if levelData.PlayerHitFrameCount <= 0 then
                        DrawPlayer.Draw e.Graphics levelData.PlayerX ((this.Size.Height - hudHeight) - PlayerHeight)
                    levelData.PlayerShots |> List.iter (DrawPlayer.DrawShot e.Graphics)
                    levelData.InvaderShots |> List.iter (DrawInvaders.DrawShot e.Graphics)

            use brush = new SolidBrush(Color.FromArgb(random.Next(0, 255), random.Next(0, 255), random.Next(0, 255)))

            match this.GameState with
            | StartScreen -> drawStringCentre brush "Spaced invaders"
            | InGame inGameState -> drawLevel inGameState


            explosions |> Seq.iter (fun e -> DrawExplosion.Draw surface e)

        member this.Tick() =

            let viewSize = Size.create (this.Size.Width) (this.Size.Height)

            this.Invalidate()

            if List.length starFields < 3 then
                starFields <- [for i in 1 .. 3 do yield StarField.create viewSize (50 - i*10) Brushes.Gray (int (3.0**float i))]
            else
                starFields <- starFields |> List.map (StarField.move random viewSize)

            let calculateExplosions (hitShots: Point seq) =
                hitShots |> Seq.iter (fun s -> explosions.Add(Particles.Explosion.create 80 s.X s.Y 5.0))

            if explosions.Count > 0 then
                {explosions.Count-1..-1..0} 
                |> Seq.iter (fun i -> 
                    explosions.[i] <- Particles.Explosion.update explosions.[i] 0.05f
                    if (Explosion.top explosions.[i]).Y > single this.Height then 
                        explosions.RemoveAt(i)
                )

            this.GameState <- GameLoop.run this.GameState (Size.create (this.Size.Width) (this.Size.Height - hudHeight)) calculateExplosions animations
                    
    end