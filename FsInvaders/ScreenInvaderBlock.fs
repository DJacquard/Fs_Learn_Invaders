namespace Invaders

type ScreenInvaderBlock = {
        Position: Point
        PixelSize: Size
        InvaderBlock: InvaderGrid
        }    
        with member this.GridSize = Size.create this.InvaderBlock.columnCount (this.InvaderBlock.rowCount)

module ScreenInvaderBlock =

    open InvaderGrid

    let private columnWidth block =
        block.PixelSize.Width / block.InvaderBlock.columnCount

    let private rowHeight block =
        block.PixelSize.Height / block.InvaderBlock.rowCount

    let leftEdge block = 
        leftmostLiveColumn block.InvaderBlock * columnWidth block + block.Position.X

    let rightEdge block = 
        (rightmostLiveColumn block.InvaderBlock  + 1) * columnWidth block + block.Position.X

    let bottomEdge block =
        (bottomLiveRow block.InvaderBlock + 1) * rowHeight block + block.Position.Y

    let topEdge block =
        topLiveRow block.InvaderBlock * rowHeight block + block.Position.Y

    let create width height columns rows =
        { 
            Position = Point.create 0 0;
            PixelSize = Size.create width height;
            InvaderBlock = InvaderGrid.create columns rows
        }

    let blockToScreen block {X = x; Y = y} =
        Point.create (x * columnWidth block + block.Position.X) (y * rowHeight block + block.Position.Y)

    let removeAt block point = 
        {block with InvaderBlock = killInvaderAtPosition block.InvaderBlock point} 




