module GameScreen

open GameLoop
open Particles
open ItemDrawing
open System.Drawing
open GameLoop.GameState
open GameParameters
open System


module FrameRate =
    type T = {
            frameCount: int
            ticksStart: int
            rate: single
        }

    let Default = { frameCount = 0; ticksStart = 0; rate = 0.0f }

    let calculate frameRate now =
        (float32 frameRate.frameCount) / float32 ((now - frameRate.ticksStart))

    let incrementFrame frameRate = {frameRate with frameCount = frameRate.frameCount + 1}

    let update frameRate now =
        if now - frameRate.ticksStart >= 2000 then
            {frameRate with ticksStart = now; rate = calculate frameRate now}
        else
            frameRate

type T = {
    gameState: GameState.State
    explosions: ParticleCloud.ParticleCloud list
    starfields: StarField.Starfield list
    frameRate: FrameRate.T
        }

let Default =
    {
        gameState = GameLoop.GameState.State.Default
        explosions = []
        starfields = []
        frameRate = FrameRate.Default
    }

let draw surface size (clientRectangle: System.Drawing.Rectangle) (graphics: Graphics) (font: Font) (random: Random) state =
    graphics.DrawString(state.frameRate.rate.ToString(), font, Brushes.White, PointF(0.0f,0.0f))

    let toRectangleF r = RectangleF.op_Implicit(r)

    let drawHud (LevelNumber level) lives =
        graphics.DrawLine(Pens.Orange, Point(0, hudHeight - 1), Point(size.Width, hudHeight - 1))
        use sf = new StringFormat()
        sf.LineAlignment <- StringAlignment.Far
        sf.Alignment <- StringAlignment.Near
        graphics.DrawString(sprintf "Level %d" level, font, Brushes.Orange, RectangleF(0.0f, 0.0f, float32 size.Width, float32 hudHeight - 1.0f), sf)

        sf.Alignment <- StringAlignment.Far
        graphics.DrawString(sprintf "Lives %d" (valueLives lives), font, Brushes.Orange, RectangleF(0.0f, 0.0f, float32 size.Width, float32 hudHeight - 1.0f), sf)

    let drawStringCentre brush s = 
        use sf = new StringFormat()
        sf.LineAlignment <- StringAlignment.Center
        sf.Alignment <- StringAlignment.Center
        graphics.DrawString(s, font, brush, toRectangleF clientRectangle, sf)

    let drawWaitScreen screenType =
        match screenType with
        | LevelIntro animation -> //drawStringCentre Brushes.Orange (sprintf "Get ready\r\nLevel: %d\r\nRemaining lives: %d" (valueLevelNumber gameState.Level) (valueLives gameState.Lives))
            Animation.draw animation
        | LevelComplete -> drawStringCentre Brushes.Yellow "Level complete"
        | GameOver -> drawStringCentre Brushes.Red "Game Over"

    let drawLevel ({LevelState = levelState; SubState = subState} as gameState) =
        drawHud subState.Level subState.Lives
        state.starfields |> List.iter (StarField.draw surface)
        match levelState with
        | WaitScreen (screenType,_) -> drawWaitScreen screenType
        | Level levelData ->
            graphics.TranslateTransform(0.0f, float32 hudHeight)            
            DrawInvaders.DrawInvaders graphics levelData.InvaderData.Invaders
            if levelData.PlayerHitFrameCount <= 0 then
                DrawPlayer.Draw graphics levelData.PlayerX ((size.Height - hudHeight) - PlayerHeight)
            levelData.PlayerShots |> List.iter (DrawPlayer.DrawShot graphics)
            levelData.InvaderShots |> List.iter (DrawInvaders.DrawShot graphics)

    use brush = new SolidBrush(Color.FromArgb(random.Next(0, 255), random.Next(0, 255), random.Next(0, 255)))

    match state.gameState with
    | StartScreen -> drawStringCentre brush "Spaced invaders"
    | InGame inGameState -> drawLevel inGameState

    state.explosions |> Seq.iter (fun e -> DrawExplosion.Draw surface e)

let update random size animations state =
    let newHits, gameState = GameLoop.run state.gameState (Size.create (size.Width) (size.Height - GameParameters.hudHeight)) animations

    let explosions = List.append state.explosions (newHits |> List.map (fun s -> Particles.Explosion.create 80 s.X s.Y 5.0))

    let explosions = explosions |> List.map (fun s -> Particles.Explosion.update s 0.05f)

    let explosions = explosions |> List.filter (fun s -> (Explosion.top s).Y <= single size.Height)

    let starfields =
        if List.length state.starfields < 3 then
                [for i in 1 .. 3 do yield StarField.create size (50 - i*10) Brushes.Gray (int (2.0**float i))]
            else
                state.starfields |> List.map (StarField.move random size)

    {
        gameState = gameState
        explosions = explosions
        starfields = starfields
        frameRate = FrameRate.update (state.frameRate) (Environment.TickCount)
    }
