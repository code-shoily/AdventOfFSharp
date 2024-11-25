/// Year 2022/4 - ???
/// Link: https://adventofcode.com/2022/day/4
/// Difficulty:
/// Tags:
/// Remarks:
module Year2022.Day04

open Common.Helpers
open Common.Types

[<AutoOpen>]
module Section =
    type Pair =
        { From: int
          To: int }

        static member fromLine(line: string) =
            match line.Split("-") with
            | [| left; right |] -> { From = int left; To = int right }
            | _ -> unreachable ()

    type SectionAssignment =
        { Left: Pair
          Right: Pair }

        static member fromLine(line: string) =
            match line.Split(",") with
            | [| leftPair; rightPair |] ->
                { Left = Pair.fromLine leftPair
                  Right = Pair.fromLine rightPair }
            | _ -> unreachable ()

    let subsetPair { Left = left; Right = right } =
        let isSubsetOf left right =
            (left.From <= right.From && left.To >= right.To)

        (isSubsetOf left right) || isSubsetOf right left

    let overlappingPair { Left = left; Right = right } =
        let isOverlapping left right =
            (left.From <= right.From && left.To >= right.From)

        (isOverlapping left right) || isOverlapping right left


let parse = Seq.map SectionAssignment.fromLine

let countSectionsBy relationship = Seq.filter relationship >> Seq.length

let solvePart1 = countSectionsBy subsetPair
let solvePart2 = countSectionsBy overlappingPair

let solve (rawInput: string seq) =
    let input = rawInput |> parse
    BothInt(solvePart1 input, solvePart2 input)
