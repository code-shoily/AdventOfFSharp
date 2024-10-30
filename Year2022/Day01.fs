/// Year 2022/1 - Calorie Counting
/// Link: https://adventofcode.com/2022/day/1
/// Difficulty: xs
/// Tags: sort
/// Remarks:
module Year2022.Day01

open Common.Helpers
open Common.Types

let parse =
    Seq.toList >> paragraphs >> List.map (ints >> Seq.sum) >> List.sortDescending

let solvePart1 = List.head
let solvePart2 = List.take (3) >> List.sum

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
