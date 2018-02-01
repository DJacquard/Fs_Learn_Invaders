module Drawing

open System.Windows.Forms
open Game

type SysPoint = System.Drawing.Point
type SysSize = System.Drawing.Size
type SysRectangle = System.Drawing.Rectangle


let PointToSys p = SysPoint(p.X, p.Y) 

let SizeToSys s = SysSize(s.Width, s.Height)

let RectangleToSys = Rectangle.apply (fun (p, s) -> SysRectangle(PointToSys p, SizeToSys s))

open System.Drawing

module DrawSurface =

    type T = Surface of Graphics * Size

    let infiniteSurface g = (g, Size.Empty) |> Surface

    let FillRectangle (Surface (g, _)) (brush: Brush) (rect: Rectangle) =
        g.FillRectangle(brush, rect)

    let inline DrawPointF (Surface (g, _)) (brush: Brush) x y =
        g.FillRectangle(brush, Rectangle(Point(int x, int y), Size(1, 1)))

    let drawRect surface brush rectangle =
        FillRectangle (infiniteSurface surface) brush (RectangleToSys rectangle)

    type DrawingFunctions = {
        drawRect: Brush->Game.DomainTypes.Rectangle->unit
        }

    let drawFunctions surface = {
        drawRect = drawRect surface
    }
