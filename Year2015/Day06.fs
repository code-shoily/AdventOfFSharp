/// Year 2015/6 - Probably a Fire Hazard
/// Link: https://adventofcode.com/2015/day/6
/// Difficulty: xs
/// Tags: mutable grid slow
/// Remarks: Optimize with range geometry?
module Year2015.Day06

open System.Text.RegularExpressions
open Common.Helpers
open Common.Types

type Instruction =
    | On of int * int * int * int
    | Off of int * int * int * int
    | Toggle of int * int * int * int

let parse =
    let parseLine (line: string) =
        let regex = Regex(@"(turn on|toggle|turn off) (\d+),(\d+) through (\d+),(\d+)")
        let values = line |> regex.Match |> _.Groups |> Seq.map _.Value |> List.ofSeq

        match values with
        | [ _; value; x1; y1; x2; y2 ] ->
            let x1, y1, x2, y2 = int x1, int y1, int x2, int y2

            match value with
            | "turn on" -> On <| (x1, y1, x2, y2)
            | "turn off" -> Off <| (x1, y1, x2, y2)
            | "toggle" -> Toggle <| (x1, y1, x2, y2)
            | _ -> unreachable ()
        | _ -> unreachable ()

    Seq.map parseLine

let solvePart1 (input: Instruction seq) =
    let grid: int[][] = Array.init 1000 (fun _ -> Array.zeroCreate 1000)

    input
    |> Seq.iter (function
        | On(x1, y1, x2, y2) ->
            [ x1..x2 ]
            |> Seq.allPairs [ y1..y2 ]
            |> Seq.iter (fun (x, y) -> grid[x][y] <- 1)
        | Off(x1, y1, x2, y2) ->
            [ x1..x2 ]
            |> Seq.allPairs [ y1..y2 ]
            |> Seq.iter (fun (x, y) -> grid[x][y] <- 0)
        | Toggle(x1, y1, x2, y2) ->
            [ x1..x2 ]
            |> Seq.allPairs [ y1..y2 ]
            |> Seq.iter (fun (x, y) -> grid[x][y] <- if grid[x][y] = 1 then 0 else 1))

    grid |> Array.collect id |> Array.filter ((=) 1) |> Array.length

let solvePart2 (input: Instruction seq) =
    let grid: int[][] = Array.init 1000 (fun _ -> Array.zeroCreate 1000)

    input
    |> Seq.iter (function
        | On(x1, y1, x2, y2) ->
            [ x1..x2 ]
            |> Seq.allPairs [ y1..y2 ]
            |> Seq.iter (fun (x, y) -> grid[x][y] <- grid[x][y] + 1)
        | Off(x1, y1, x2, y2) ->
            [ x1..x2 ]
            |> Seq.allPairs [ y1..y2 ]
            |> Seq.iter (fun (x, y) -> grid[x][y] <- max 0 (grid[x][y] - 1))
        | Toggle(x1, y1, x2, y2) ->
            [ x1..x2 ]
            |> Seq.allPairs [ y1..y2 ]
            |> Seq.iter (fun (x, y) -> grid[x][y] <- grid[x][y] + 2))

    grid |> Array.collect id |> Array.sum

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
