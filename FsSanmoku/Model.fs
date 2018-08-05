namespace FsSanmoku.Core

module Model = begin

    type Player =
    | Player1 = 0
    | Player2 = 1

    type Cell ={
        hasStone:bool
        stone:Player
        index:int
    }

    let isFilled line :bool =
        line
        |> List.map(fun cell -> cell.hasStone)
        |> List.fold (fun acc x -> acc && x) true

    let areSame line :bool =
        line
        |> List.map(fun cell -> cell.stone)
        |> List.fold(fun acc x -> acc && (x=line.Head.stone)) true

    let hasWinner lines :bool =
        lines 
        |> List.filter(fun line -> isFilled line)
        |> List.exists(fun line -> areSame line)

    let winner lines :Player =
        (lines |> List.find(fun line -> areSame line)).Head.stone
        
    let lin1 x = x/3=0 
    let lin2 x = x/3=1 
    let lin3 x = x/3=2 
    let col1 x = x%3=0 
    let col2 x = x%3=1 
    let col3 x = x%3=2
    let sln1 x = x=0||x=4||x=8
    let sln2 x = x=2||x=4||x=6

    let lines cells :List<List<Cell>> =
        [lin1; lin2; lin3; col1; col2; col3; sln1; sln2]
        |> List.map(fun pred ->(cells |> List.filter(fun cell -> pred cell.index)))

    let isBoardFilled cells :bool = 
        cells |> List.forall(fun cell -> cell.hasStone)


end

