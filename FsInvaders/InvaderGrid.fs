namespace Invaders

type InvaderGrid = {
        columnCount: int
        rowCount: int
        invaders: Invader list
}

module InvaderGrid =

    let private columnsContainingLiveInvaders columnCount invaders =
        invaders
        |> List.mapi (fun i inv -> (i % columnCount, inv))
        |> List.filter (fun (_, inv) -> inv |> Invader.isAlive)
        |> List.map (fun (i, _) -> i)

    let private rowsContainingLiveInvaders columnCount invaders =
        invaders
        |> List.mapi (fun i inv -> (i / columnCount, inv))
        |> List.filter (fun (_, inv) -> inv |> Invader.isAlive)
        |> List.map (fun (i, _) -> i)


    let numberOfLiveInvaders block =
        block.invaders 
        |> List.filter Invader.isAlive
        |> List.length

    let isEmpty block = numberOfLiveInvaders block <= 0

    let leftmostLiveColumn block =
        block.invaders 
        |> columnsContainingLiveInvaders block.columnCount
        |> List.min

    let rightmostLiveColumn block =
        block.invaders 
        |> columnsContainingLiveInvaders block.columnCount
        |> List.max

    let bottomLiveRow block =
        block.invaders 
        |> rowsContainingLiveInvaders block.columnCount
        |> List.max

    let topLiveRow block =
        block.invaders
        |> rowsContainingLiveInvaders block.columnCount
        |> List.min

    let allAliveInPosition block =
        block.invaders 
        |> List.mapi (fun i inv -> (i, inv |> Invader.isAlive))
        |> List.filter (fun (_, isAlive) -> isAlive)
        |> List.map (fun (index, _) -> Point.create (index % block.columnCount) (index / block.columnCount))

    let killInvaderAtPosition block {X = x; Y = y} =
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


