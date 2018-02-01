module ItemDrawing

open System.Drawing
open Drawing.DrawSurface
open Drawing

module DrawInvaders =
    open Invaders

    let Draw surface point =
        FillRectangle surface Brushes.LightGreen (SysRectangle(PointToSys point, Size(GameParameters.InvaderSize, GameParameters.InvaderSize)))

    let DrawInvaders surface invaders =
        let Draw p = Draw surface p

        invaders |> InvaderBlock.Iterate (Draw << Invader.location)

    let DrawShot surface shot =
        shot |> InvaderShots.apply (fun r -> FillRectangle surface Brushes.Yellow (RectangleToSys r))
    

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