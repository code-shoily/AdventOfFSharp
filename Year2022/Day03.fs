/// Year 2022/3 - Rucksack Reorganization
/// Link: https://adventofcode.com/2022/day/3
/// Difficulty: s
/// Tags: set ascii
/// Remarks: Why can't extract the iteration of common items?
module Year2022.Day03

open Common.Helpers
open Common.Types

let getPriority =
    function
    | c when 'a' <= c && c <= 'z' -> (int c % int 'a') + 1
    | c when 'A' <= c && c <= 'Z' -> (int c % int 'A') + 27
    | _ -> unreachable ()

let commonItem = Seq.reduce Set.intersect >> Seq.head

let solvePart1 =
    Seq.map (Seq.splitInto 2 >> (Seq.map Set.ofSeq) >> commonItem >> getPriority)
    >> Seq.sum

let solvePart2 =
    Seq.chunkBySize 3
    >> Seq.map ((Seq.map Set.ofSeq) >> commonItem >> getPriority)
    >> Seq.sum

let solve (rawInput: string seq) =
    BothInt(solvePart1 rawInput, solvePart2 rawInput)
