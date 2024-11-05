/// Year 2020/3 - Toboggan Trajectory
/// Link: https://adventofcode.com/2020/day/3
/// Difficulty: xs
/// Tags: navigation
/// Remarks:
module Year2020.Day03

open Common.Helpers
open Common.Types

type Terrain =
    | Tree
    | Square

let parse =
    let toTerrain =
        function
        | '.' -> Square
        | '#' -> Tree
        | _ -> unreachable ()

    Seq.map (Seq.map toTerrain >> Seq.toArray) >> Array.ofSeq

let treeCount (right, down) (area: Terrain[][]) =
    let width, height = area[0].Length, area.Length

    let rec run (row, col) trees =
        if row >= height then
            trees
        else
            let encounter = (if area[row][col] = Tree then 1 else 0)
            run (row + down, (col + right) % width) trees + encounter

    int64 <| run (0, 0) 0

let solvePart1 = treeCount (3, 1)

let solvePart2 input : int64 =
    (1L,
     [ treeCount (1, 1)
       treeCount (3, 1)
       treeCount (5, 1)
       treeCount (7, 1)
       treeCount (1, 2) ])
    ||> List.fold (fun acc slope -> acc * slope input)

let solve (rawInput: string seq) =
    let input = rawInput |> parse
    BothLong(solvePart1 input, solvePart2 input)
