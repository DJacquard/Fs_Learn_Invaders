module WinFormsTypes

open System.Windows.Forms

type SysPoint = System.Drawing.Point
type SysSize = System.Drawing.Size
type SysRectangle = System.Drawing.Rectangle
type SysPointF = System.Drawing.PointF


let PointToSys p = SysPoint(p.X, p.Y) 

let PointToSysF p = SysPointF(float32 p.X, float32 p.Y)

let SizeToSys s = SysSize(s.Width, s.Height)

let RectangleToSys = Rectangle.apply (fun (p, s) -> SysRectangle(PointToSys p, SizeToSys s))



module KeyMatrix =

    type Matrix =
        {
            RegisteredKeys: Set<Keys>
            CurrentPressed: Set<Keys>
        }
        with static member Create registeredKeys = { RegisteredKeys = registeredKeys; CurrentPressed = Set.empty; }

    let registerKey matrix code =
        {matrix with RegisteredKeys = matrix.RegisteredKeys.Add(code)}

    let pressKey matrix code =
        match matrix.RegisteredKeys.Contains(code) with 
        | false -> matrix
        | true -> {matrix with CurrentPressed = matrix.CurrentPressed.Add(code)}

    let releaseKey matrix code =
        match matrix.RegisteredKeys.Contains(code) with
        | false -> matrix
        | true -> {matrix with CurrentPressed = matrix.CurrentPressed.Remove(code)}

    let isKeyPressed matrix code =
        matrix.CurrentPressed.Contains(code)

    let doKeyState matrix code state pressed notPressed =
        match isKeyPressed matrix code with
        | true -> pressed state
        | _ -> notPressed state

 module ItemDrawing =

    open System.Drawing

    let FillRectangle (g: Graphics) (brush: Brush) (rect: Rectangle) =
        g.FillRectangle(brush, rect)

    let inline DrawPointF g brush x y =
        FillRectangle g brush (SysRectangle(Point(int x, int y), Size(1, 1)))


    module DrawInvaders =
        open Invaders
    

        let Draw surface point =
            FillRectangle surface Brushes.LightGreen (SysRectangle(PointToSys point, Size(GameParameters.InvaderSize, GameParameters.InvaderSize)))

        let DrawInvaders surface invaders =
            let Draw p = 
                Draw surface (Invaders.ScreenInvaderBlock.blockToScreen invaders p)

            invaders.InvaderBlock |> InvaderGrid.allAliveInPosition |> List.iter Draw

        let DrawShot surface shot =
            shot |> InvaderShot.apply (fun r -> FillRectangle surface Brushes.Yellow (RectangleToSys r))
    

    module DrawPlayer =
        open GameParameters

        let Draw surface position height =
            FillRectangle surface Brushes.Gray (SysRectangle(Point(position, height), Size(PlayerWidth, PlayerHeight)))

        let DrawShot surface point =
            FillRectangle surface Brushes.Red (SysRectangle(PointToSys point, Size(2, 20)))

    module DrawExplosion =
        open Particles.ParticleCloud
        open Particles.Particle
        let Draw surface particles =
            let drawParticle p = DrawPointF surface Brushes.Yellow p.X p.Y
            particles |> iterate drawParticle

    
    let brightnessList =
        [|   new SolidBrush(Color.FromArgb(255, 150, 150, 180));
            new SolidBrush(Color.FromArgb(255, 170, 170, 200));
            new SolidBrush(Color.FromArgb(255, 220, 130, 220));
            new SolidBrush(Color.FromArgb(255, 220, 220, 245));
            new SolidBrush(Color.FromArgb(255, 245, 245, 245))
        |]

    let drawStarfield surface (StarField.Starfield (_, points)) =
        points |> List.iter (fun ({X = x; Y = y}, intensity) -> DrawPointF surface brightnessList.[intensity] x y)

 module Renderer =
    open System
    open System.Drawing
    open GameScreen
    open GameLoop.GameState
    open GameParameters
    open ItemDrawing

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
            state.starfields |> List.iter (drawStarfield surface)
            match levelState with
            | WaitScreen (screenType,_) -> drawWaitScreen screenType
            | Level levelData ->
                graphics.TranslateTransform(0.0f, float32 hudHeight)            
                DrawInvaders.DrawInvaders graphics levelData.InvaderData.Invaders
                if levelData.PlayerHitFrameCount <= 0 then
                    DrawPlayer.Draw graphics levelData.Player.Position ((size.Height - hudHeight) - PlayerHeight)
                levelData.Player.Shots |> List.iter (DrawPlayer.DrawShot graphics)
                levelData.InvaderShots |> List.iter (DrawInvaders.DrawShot graphics)

        use brush = new SolidBrush(Color.FromArgb(random.Next(0, 255), random.Next(0, 255), random.Next(0, 255)))

        match state.gameState with
        | StartScreen -> drawStringCentre brush "Spaced invaders"
        | InGame inGameState -> drawLevel inGameState

        state.explosions |> Seq.iter (fun e -> DrawExplosion.Draw surface e)

module Animations = 

    open Animation

    open GameLoop.GameState

    open System.Drawing

    let testAnimation (graphicsRef: Ref<Graphics>) inGameState =
    
        let numberOfLives = inGameState.Lives |> valueLives

        let font = new Font(new FontFamily("Arial"), 16.0f, FontStyle.Bold, GraphicsUnit.Pixel)

        let drawRemainingText pos =
            graphicsRef.Value.DrawString("Remaining ships:", font, System.Drawing.Brushes.Orange, PointToSysF pos)   
        
        let remainingText = createCharacter {X = 50; Y = 100} true drawRemainingText

        let drawShip pos = 
            ItemDrawing.DrawPlayer.Draw !graphicsRef pos.X pos.Y

        let ship startVis = createCharacter {X = 0; Y = 120} startVis drawShip

        let createShipChar x =
            let startFrame = x * 25
            let frames = startFrame + 24 - x * 5
            [(startFrame, transformVisibility true); (startFrame, transformXVelocity 10); (frames, transformXVelocity 0)] |> setKeyframes (ship false)


        let shipChars = [for x in 0..numberOfLives - 1 -> createShipChar x]

        createAnimation 180 
           ( List.append [
                         [(0, transformXVelocity 0)] |> setKeyframes (remainingText)
                        ]
                        shipChars)

