module Scoring

module Score =
    [<Struct>]
    type Score = Score of uint32

    let map f (Score score) = f(score) |> Score

    let add score = map ((+) score)

    let value (Score score) = score

module HighScoreTable = 
    open Score

    type Player = Player of char * char * char
    
    [<CustomComparisonAttribute>]
    [<CustomEqualityAttribute>]
    type Entry = Entry of Player * Score
        with 
        
        member x.Score() = 
                let f (Entry (_, s)) = s
                f x

        override x.Equals(yobj) = 
            match yobj with
            | :? Entry as y -> compare x y = 0
            | _ -> false

        override x.GetHashCode() = x.Score().GetHashCode()

        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? Entry as y -> compare (x.Score()) (y.Score())
                | _ -> failwith "type mismatch"
                

    type Table = Table of Entry list

    let createEntry (first, second, third) score = ((first, second, third) |> Player, score) |> Entry

    let compareScoreToEntry score (Entry (_, eScore)) = score > eScore

    let createTable entries = entries |> Seq.sortDescending |> Seq.take 10 |> Seq.toList |> Table

    let fold f (Table t) = f t

    let isHighScore score t = compareScoreToEntry score (t |> fold List.min)

    let insertScore entry t = t |> fold (fun l -> entry::l) |> createTable
