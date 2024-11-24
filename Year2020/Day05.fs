/// Year 2020/5 - Binary Boarding
/// Link: https://adventofcode.com/2020/day/5
/// Difficulty: xs
/// Tags: binary
/// Remarks:
module Year2020.Day05

open System
open Common.Types

let parse =
    let getSeatID =
        Seq.mapi (fun i ->
            function
            | 'B'
            | 'R' -> Math.Pow(2, double <| 9 - i) |> int
            | _ -> 0)
        >> Seq.sum

    Seq.map getSeatID

let solvePart1 = Seq.maxBy id

let solvePart2 =
    Seq.sort
    >> Seq.pairwise
    >> Seq.find (fun (a, b) -> b > a && b - a <> 1)
    >> (fst >> (+) 1)

let solve (rawInput: string seq) =
    let input = rawInput |> parse
    BothInt(solvePart1 input, solvePart2 input)
