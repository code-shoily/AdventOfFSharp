/// Year 2018/7 - The Sum of Its Parts
/// Link: https://adventofcode.com/2018/day/7
/// Difficulty: xs
/// Tags: graph topological-sort difficult-part-2
/// Remarks:
module Year2018.Day07

open System
open Common.Types
open Yog
open Yog.Builder

let parse (rawInput: string seq) =
    rawInput
    |> Seq.map (fun line ->
        let words = line.Split(' ')
        words.[1], words.[7])
    |> Seq.fold (fun b (prereq, step) -> b |> Labeled.addSimpleEdge prereq step) (Labeled.directed<string, int> ())
    |> Labeled.toGraph

let solvePart1 (graph: Model.Graph<string, int>) =
    match Traversal.lexicographicalTopologicalSort (fun a b -> String.Compare(a, b)) graph with
    | Ok order ->
        order
        |> List.choose (fun id -> Map.tryFind id graph.Nodes)
        |> String.concat ""
    | Error () -> ""

let solvePart2 (_graph: Model.Graph<string, int>) = 0

let solve (rawInput: string seq) =
    let input = parse rawInput
    StringInt(solvePart1 input, solvePart2 input)
