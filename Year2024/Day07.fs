/// Year 2024/7 - Bridge Repair
/// Link: https://adventofcode.com/2024/day/7
/// Difficulty: xs
/// Tags: recursion slightly-slow equation
/// Remarks:
module Year2024.Day07

open System
open Common.Helpers
open Common.Types

let parse (rawInput: string seq) =
    let parseLine: (string[] -> int64 * int64 list) =
        function
        | [| left; right |] -> (int64 left, (right |> _.Split(" ") |> Array.map int64 |> List.ofArray))
        | _ -> unreachable ()

    rawInput
    |> Seq.map (_.Split(":", StringSplitOptions.TrimEntries) >> parseLine)
    |> List.ofSeq

let isProducing ops (testValue: int64) (vals) =
    let rec getProducers (testValue: int64) =
        function
        | [ x ] -> testValue = x
        | leftOp :: rightOp :: rest ->
            Seq.fold (fun res op -> (getProducers testValue <| ((leftOp, rightOp) ||> op) :: rest) || res) false ops
        | _ -> false

    getProducers testValue vals

let solvePart1 =
    Seq.filter (fun data -> data ||> isProducing [ (+); (*) ]) >> Seq.sumBy fst

let solvePart2 =
    let inline (<+>) (left: int64) (right: int64) = $"{left}{right}" |> int64

    Seq.filter (fun data -> data ||> isProducing [ (+); (*); (<+>) ])
    >> Seq.sumBy fst

let solve (input: string seq) =
    let input = parse input
    BothLong(solvePart1 input, solvePart2 input)
