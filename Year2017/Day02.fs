/// Year 2017/2 - Corruption Checksum
/// Link: https://adventofcode.com/2017/day/2
/// Difficulty: xs
/// Tags: arithmetic
/// Remarks: Optimize it so `allPairs` is not needed.
module Year2017.Day02

open Common.Types

let parse = Seq.map (fun (row: string) -> row.Split '\t' |> Seq.map int)

let solvePart1 = Seq.map (fun row -> (Seq.max row) - Seq.min row) >> Seq.sum

let solvePart2 =
    let evenlyDivides row =
        row |> Seq.allPairs row |> Seq.find (fun (a, b) -> a > b && a % b = 0)

    Seq.map (fun row -> evenlyDivides row ||> (/)) >> Seq.sum

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
