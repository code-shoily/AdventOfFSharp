/// Year 2024/1 - Historian Hysteria
/// Link: https://adventofcode.com/2024/day/1
/// Difficulty: xs
/// Tags: sort frequency
/// Remarks: 
module Year2024.Day01

open System
open Common.Helpers
open Common.Types

let parse (input: string seq) =
    ((List.empty, List.empty), input)
    ||> Seq.fold (fun (leftList, rightList) line ->
        match line.Split(" ", StringSplitOptions.RemoveEmptyEntries) with
        | [| left; right |] -> (int left) :: leftList, (int right) :: rightList
        | _ -> unreachable ())

let solvePart1 (leftList, rightList) =
    (List.sort leftList, List.sort rightList)
    ||> Seq.zip
    |> Seq.map (fun (a, b) -> abs <| a - b)
    |> Seq.sum

let solvePart2 (leftList, rightList) =
    let frequencies =
        rightList
        |> Seq.groupBy id
        |> Seq.map (fun (k, vals) -> (k, Seq.length vals))
        |> Map.ofSeq

    let similarityScore idValue =
        match frequencies |> Map.tryFind idValue with
        | Some frequency -> idValue * frequency
        | None -> 0

    leftList |> Seq.map similarityScore |> Seq.sum

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
