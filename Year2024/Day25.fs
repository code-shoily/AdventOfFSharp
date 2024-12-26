/// Year 2024/25 - Code Chronicle
/// Link: https://adventofcode.com/2024/day/25
/// Difficulty: s
/// Tags: data-transform
/// Remarks:
module Year2024.Day25

open Common.Helpers
open Common.Types

type Schematic =
    | Lock of int list
    | Key of int list

    static member fromChars =
        let heights = List.map (List.filter ((=) '#') >> List.length >> (fun v -> v - 1))

        function
        | ('.' :: _) :: _ as chars -> Lock(heights chars)
        | chars -> Key(heights chars)

let parse =
    let transform = List.map List.ofSeq >> List.transpose >> Schematic.fromChars
    Seq.toList >> paragraphs >> List.map transform

let solvePart1 (schematics: Schematic list) =
    let lockFitsKey =
        function
        | Lock a, Key b -> (a, b) ||> List.map2 (fun a b -> a + b <= 5) |> List.forall id
        | _ -> false

    schematics |> List.allPairs schematics |> List.filter lockFitsKey |> List.length

let solve (rawInput: string seq) =
    let input = parse rawInput
    IntString(solvePart1 input, "🎉")
