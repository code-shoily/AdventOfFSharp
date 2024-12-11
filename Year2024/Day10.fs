/// Year 2024/10 - Hoof It
/// Link: https://adventofcode.com/2024/day/10
/// Difficulty: xs
/// Tags: graph-traversal map-grid
/// Remarks:
module Year2024.Day10

open Common.Types

type TrailMap =
    { Heights: Map<int * int, int> }

    member this.Neighbours(x, y) =
        [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
        |> List.filter (fun point ->
            match this.Heights.TryFind point with
            | Some(height) -> this.Heights[(x, y)] = height - 1
            | None -> false)

    member this.TrailHeads =
        this.Heights
        |> Seq.choose (fun kv -> if kv.Value = 0 then Some(kv.Key) else None)

    static member New =
        Seq.mapi (fun v row -> row |> Seq.mapi (fun h height -> ((v, h), height |> string |> int)))
        >> Seq.collect id
        >> (fun data -> { Heights = Map.ofSeq data })

    member this.findAllTrails =
        let rec findTrailsFor height point =
            match height with
            | 9 -> [ point ]
            | height -> this.Neighbours point |> List.collect (findTrailsFor (height + 1))

        this.TrailHeads |> Seq.map (findTrailsFor 0)

let solvePart1 = Seq.sumBy (Seq.distinct >> Seq.length)
let solvePart2 = Seq.sumBy Seq.length

let parse = TrailMap.New >> _.findAllTrails

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
