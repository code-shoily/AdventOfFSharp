/// Year 2020/7 - Handy Haversacks
/// Link: https://adventofcode.com/2020/day/7
/// Difficulty: s
/// Tags: graph dfs
/// Remarks: Uses Yog.FSharp for graph operations
module Year2020.Day07

open System
open System.Collections.Generic
open Common.Helpers
open Common.Types
open Yog.Model
open Yog.Traversal

[<AutoOpen>]
module Parser =
    let getBags (bagCount: string) =
        let trimSuffices (s: string) =
            s.Replace(".", "").Replace("bags", "").Replace("bag", "").Trim()

        let getBagInfo (info: string) =
            match info.Split(" ") |> List.ofArray with
            | "no" :: rest -> trimSuffices <| String.Concat(rest), 0
            | count :: rest -> trimSuffices <| String.Concat(rest), int count
            | _ -> unreachable ()

        bagCount.Split(",", StringSplitOptions.TrimEntries)
        |> Seq.map getBagInfo
        |> Seq.filter (fun (_, count) -> count > 0)

    let parseBag (bagInfo: string) =
        match bagInfo.Split("bags contain", StringSplitOptions.TrimEntries) with
        | [| source; rest |] ->
            let sourceKey = source.Replace(" ", "")
            let contained = getBags rest |> Seq.toList
            sourceKey, contained
        | _ -> unreachable ()

    let buildIdMap (lines: string seq) : Map<string, int> =
        let idMap = Dictionary<string, int>()
        let mutable nextId = 1
        
        let getOrAddId name =
            if not (idMap.ContainsKey(name)) then
                idMap.[name] <- nextId
                nextId <- nextId + 1
            idMap.[name]
        
        for line in lines do
            let source, contained = parseBag line
            getOrAddId source |> ignore
            for (name, _) in contained do
                getOrAddId name |> ignore
        
        idMap |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq

    let buildGraph (idMap: Map<string, int>) (lines: string seq) : Graph<int, int> =
        let folder graph (source, contained) =
            let sourceId = idMap.[source]
            let graphWithNode = addNode sourceId sourceId graph
            (graphWithNode, contained)
            ||> Seq.fold (fun g (target, count) ->
                let targetId = idMap.[target]
                let gWithTarget = addNode targetId targetId g
                addEdge sourceId targetId count gWithTarget)

        lines
        |> Seq.map parseBag
        |> Seq.fold folder (empty Directed)

    let buildTransposedGraph (idMap: Map<string, int>) (lines: string seq) : Graph<int, int> =
        let folder graph (source, contained) =
            let sourceId = idMap.[source]
            let graphWithNode = addNode sourceId sourceId graph
            (graphWithNode, contained)
            ||> Seq.fold (fun g (target, count) ->
                let targetId = idMap.[target]
                let gWithTarget = addNode targetId targetId g
                addEdge targetId sourceId count gWithTarget)

        lines
        |> Seq.map parseBag
        |> Seq.fold folder (empty Directed)

let solvePart1 (transposedGraph: Graph<int, int>) shinyGoldId =
    let reachableNodes =
        walk shinyGoldId DepthFirst transposedGraph
        |> Set.ofList

    reachableNodes.Count - 1

let solvePart2 (graph: Graph<int, int>) shinyGoldId =
    let rec countBagsInside bagId =
        match Map.tryFind bagId graph.OutEdges with
        | None -> 0
        | Some outEdges ->
            outEdges
            |> Map.toSeq
            |> Seq.sumBy (fun (containedId, count) ->
                count + count * countBagsInside containedId)

    countBagsInside shinyGoldId

let solve (rawInput: string seq) =
    let lines = rawInput |> Seq.toList
    let idMap = Parser.buildIdMap lines
    let shinyGoldId = idMap.["shinygold"]
    let graph = Parser.buildGraph idMap lines
    let transposedGraph = Parser.buildTransposedGraph idMap lines

    BothInt(solvePart1 transposedGraph shinyGoldId, solvePart2 graph shinyGoldId)
