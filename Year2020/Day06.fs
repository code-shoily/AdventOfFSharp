/// Year 2020/6 - Custom Customs
/// Link: https://adventofcode.com/2020/day/6
/// Difficulty: xs
/// Tags: set
/// Remarks:
module Year2020.Day06

open Common.Helpers
open Common.Types

let parse = List.ofSeq >> paragraphs

let countQuestionsBasedOn reducer =
    List.map (List.map Set.ofSeq >> List.reduce reducer >> Seq.length) >> List.sum

let solvePart1 = countQuestionsBasedOn Set.union
let solvePart2 = countQuestionsBasedOn Set.intersect

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
