namespace Invaders

open Game

module Invader =
    let fromValue value = Invader value

    let create() = Invader true

    let value (Invader invader) = invader

    let apply f (Invader invader) = f invader |> Invader

    let isAlive = value

type Invader = Invader<bool>
    
open GameParameters

module InvaderGrid =
    type InvaderGrid = {
            columnCount: int
            rowCount: int
            invaders: Invader list
    }

    let NumberOfInvaders block =
        block.invaders 
        |> List.filter Invader.isAlive
        |> List.length

    let columnsContainingLiveInvaders columnCount invaders =
        invaders
        |> List.mapi (fun i inv -> (i % columnCount, inv))
        |> List.filter (fun (_, inv) -> inv |> Invader.isAlive)
        |> List.map (fun (i, _) -> i)

    let rowsContainingLiveInvaders columnCount invaders =
        invaders
        |> List.mapi (fun i inv -> (i / columnCount, inv))
        |> List.filter (fun (_, inv) -> inv |> Invader.isAlive)
        |> List.map (fun (i, _) -> i)

    let LeftmostColumn block =
        block.invaders 
        |> columnsContainingLiveInvaders block.columnCount
        |> List.min

    let RightmostColumn block =
        block.invaders 
        |> columnsContainingLiveInvaders block.columnCount
        |> List.max

    let BottomRow block =
        block.invaders 
        |> rowsContainingLiveInvaders block.columnCount
        |> List.max

    let TopRow block =
        block.invaders
        |> rowsContainingLiveInvaders block.columnCount
        |> List.min

    let allAliveInPosition block =
        block.invaders 
        |> List.mapi (fun i inv -> (i, inv |> Invader.isAlive))
        |> List.filter (fun (_, isAlive) -> isAlive)
        |> List.map (fun (index, _) -> Point.create (index % block.columnCount) (index / block.columnCount))

    let removeAt block {X = x; Y = y} =
        let index = y * block.columnCount + x

        {
            block with invaders =
                        block.invaders
                        |> List.mapi (fun i inv -> if i = index then false |> Invader.fromValue else inv)
        }

    let create columns rows =
        { 
            columnCount = columns;
            rowCount = rows;
            invaders = List.init (rows * columns) (fun _ -> Invader.create())
        }


module ScreenInvaderBlock =

    open InvaderGrid

    type ScreenInvaderBlock = {
            position: Point
            pixelSize: Size
            invaderBlock: InvaderGrid
         }    
         with member this.gridSize = Size.create this.invaderBlock.columnCount (this.invaderBlock.rowCount)

    let ColumnWidth block =
        block.pixelSize.Width / block.invaderBlock.columnCount

    let RowHeight block =
        block.pixelSize.Height / block.invaderBlock.rowCount

    let LeftEdge block = 
        LeftmostColumn block.invaderBlock * ColumnWidth block + block.position.X

    let RightEdge block = 
        (RightmostColumn block.invaderBlock  + 1) * ColumnWidth block + block.position.X

    let BottomEdge block =
        (BottomRow block.invaderBlock + 1) * RowHeight block + block.position.Y

    let TopEdge block =
        TopRow block.invaderBlock * RowHeight block + block.position.Y

    let Create width height columns rows =
        { 
            position = Point.create 0 0;
            pixelSize = Size.create width height;
            invaderBlock = InvaderGrid.create columns rows
        }

    let blockToScreen block {X = x; Y = y} =
        Point.create (x * ColumnWidth block + block.position.X) (y * RowHeight block + block.position.Y)

    let screenToBlock block {X = x; Y = y} =
        Point.create ((x - block.position.X) / ColumnWidth block) ((y - block.position.Y) / RowHeight block)

    let CalculateInvaderArea invaders =
        let x = LeftEdge invaders
        let y = TopEdge invaders
        let height = BottomEdge invaders - y
        let width = RightEdge invaders - x
        Size.create width height |> (Point.create x y |> Rectangle.create)

    let AreaAtLeftEdge area = (area |> Rectangle.leftEdge) - InvaderMove < 0

    let AreaAtRightEdge totalWidth area = (area |> Rectangle.rightEdge) >= totalWidth

    let NumberOfInvaders block = NumberOfInvaders block.invaderBlock

    let allAliveInPosition block = allAliveInPosition block.invaderBlock

    let removeAt block point = 
        {block with invaderBlock = removeAt block.invaderBlock point} 

    let isEmpty block =
        block |> NumberOfInvaders <= 0



