/// Year 2018/7 -
/// Link: https://adventofcode.com/2018/day/7
/// Difficulty:
/// Tags:
/// Remarks:
module Year2018.Day07

open System
open Common.Helpers
open Common.Types
open Yog.Model
open Yog.Builder
open Yog.Traversal

let parse (rawInput: string seq) =
    rawInput
    |> Seq.map (fun line ->
        let words = line.Split(' ')
        words.[1], words.[7])
    |> Seq.fold (fun b (prereq, step) -> b |> Labeled.addSimpleEdge prereq step) (Labeled.directed<string, int> ())
    |> Labeled.toGraph

let solvePart1 (graph: Graph<string, int>) =
    match lexicographicalTopologicalSort (fun a b -> String.Compare(a, b)) graph with
    | Ok order ->
        order
        |> List.choose (fun id -> Map.tryFind id graph.Nodes)
        |> String.concat ""
    | Error () -> ""

let solvePart2 (graph: Graph<string, int>) = 0

let solve (rawInput: string seq) =
    let input = parse rawInput
    StringInt(solvePart1 input, solvePart2 input)
