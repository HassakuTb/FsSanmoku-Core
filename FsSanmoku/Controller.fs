namespace FsSanmoku.Core

module Controller = begin
    open FsSanmoku.Core
    open Model
    open System

    type GameOver =
    | Continue = 0
    | Player1 = 1
    | Player2 = 2
    | Draw = 3
    
    let initBoard :List<Cell> =
        [0..8]
        |> List.map(fun i ->
            {
                hasStone = false
                stone = Player.Player1
                index = i
            })

    let putStone cells player index :Tuple<List<Cell>, Player> =
        (
            cells
            |> List.mapi(
                fun i cell ->
                    if i=index then
                        {
                            hasStone = true
                            stone = player
                            index = i
                        }
                    else cell
                )
            ,
            match player with
            | Player.Player1 -> Player.Player2
            | Player.Player2 -> Player.Player1
            | _-> failwith "Unknown Player"
        )

    let judge cells :GameOver =
        if hasWinner (lines cells) then
            match winner (lines cells) with
            | Player.Player1 -> GameOver.Player1
            | Player.Player2 -> GameOver.Player2
            | _-> failwith "Unknown Player"
        elif isBoardFilled cells then GameOver.Draw
        else GameOver.Continue


end

