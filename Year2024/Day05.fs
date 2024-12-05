/// Year 2024/5 - Print Queue
/// Link: https://adventofcode.com/2024/day/5
/// Difficulty: s
/// Tags: sort set
/// Remarks: 
module Year2024.Day05

open Common.Helpers
open Common.Types

[<AutoOpen>]
module Helpers =
    let median (update: int[]) = update[update.Length / 2]

    let asTuple =
        function
        | [ a; b ] -> (a, b)
        | _ -> unreachable ()

    let updateIsValid (left, right) = left = right

    let buildPrecedenceTable: (string list -> Set<int * int>) =
        List.map _.Split("|")
        >> List.map (List.ofArray >> List.map int >> asTuple)
        >> Set.ofList

    let buildPageUpdateOrders: (string list -> int[] list) =
        List.map (_.Split(",") >> Array.map int)


let parse =
    List.ofSeq
    >> paragraphs
    >> asTuple
    >> (fun (a, b) -> buildPrecedenceTable a, buildPageUpdateOrders b)

let getCurrentAndDesiredUpdates (table: Set<int * int>) =
    let sorter (left: int) (right: int) =
        if table.Contains(left, right) then -1
        elif table.Contains(right, left) then 1
        else 0

    List.map (fun update -> (update, update |> Array.sortWith sorter))

let solvePart1 = List.filter updateIsValid >> List.sumBy (fst >> median)
let solvePart2 = List.filter (updateIsValid >> not) >> List.sumBy (snd >> median)

let solve (rawInput: string seq) =
    let input = rawInput |> parse ||> getCurrentAndDesiredUpdates
    BothInt(solvePart1 input, solvePart2 input)
