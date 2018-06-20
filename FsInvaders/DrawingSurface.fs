module Drawing

open System.Windows.Forms
open Game

type SysPoint = System.Drawing.Point
type SysSize = System.Drawing.Size
type SysRectangle = System.Drawing.Rectangle
type SysPointF = System.Drawing.PointF


let PointToSys p = SysPoint(p.X, p.Y) 

let PointToSysF p = SysPointF(float32 p.X, float32 p.Y)

let SizeToSys s = SysSize(s.Width, s.Height)

let RectangleToSys = Rectangle.apply (fun (p, s) -> SysRectangle(PointToSys p, SizeToSys s))

open System.Drawing

module DrawSurface =

    type T = Surface of Graphics * Size

    let FillRectangle (g: Graphics) (brush: Brush) (rect: Rectangle) =
        g.FillRectangle(brush, rect)

    let inline DrawPointF (Surface (g, _)) (brush: Brush) x y =
        g.FillRectangle(brush, Rectangle(Point(int x, int y), Size(1, 1)))


