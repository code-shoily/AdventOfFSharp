/// Year 2024/5 - Print Queue
/// Link: https://adventofcode.com/2024/day/5
/// Difficulty: s
/// Tags: graph topological-sort
/// Remarks:
module Year2024.Day05

open Common.Helpers
open Common.Types
open Yog
open Yog.Builder

[<AutoOpen>]
module Helpers =
    let median (update: int[]) = update[update.Length / 2]

    let asTuple =
        function
        | [ a; b ] -> (a, b)
        | _ -> unreachable ()

    let parseRules: (string list -> (int * int) list) =
        List.map (_.Split("|") >> List.ofArray >> List.map int >> asTuple)

    let parseUpdates: (string list -> int list list) =
        List.map (_.Split(",") >> List.ofArray >> List.map int)

    let reorder (rules: (int * int) list) (update: int list) =
        let updateSet = Set.ofList update

        let builder =
            rules
            |> List.filter (fun (a, b) -> updateSet.Contains a && updateSet.Contains b)
            |> List.fold (fun builder (a, b) -> Labeled.addSimpleEdge a b builder) (Labeled.directed<int, int> ())

        let graph =
            update |> List.fold (fun b p -> Labeled.addNode p b) builder |> Labeled.toGraph

        match Traversal.topologicalSort graph with
        | Ok sortedIds ->
            sortedIds
            |> List.map (fun id ->
                match Map.tryFind id graph.Nodes with
                | Some label -> label
                | None -> unreachable ())
        | Error () -> unreachable ()

    let isValid rules update = update = reorder rules update

let parse =
    List.ofSeq
    >> paragraphs
    >> asTuple
    >> (fun (a, b) -> parseRules a, parseUpdates b)

let solve (rawInput: string seq) =
    let rules, updates = parse rawInput

    let part1 =
        updates
        |> List.filter (isValid rules)
        |> List.sumBy (List.toArray >> median)

    let part2 =
        updates
        |> List.filter (isValid rules >> not)
        |> List.map (reorder rules)
        |> List.sumBy (List.toArray >> median)

    BothInt(part1, part2)
