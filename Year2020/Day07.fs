/// Year 2020/7 - Handy Haversacks
/// Link: https://adventofcode.com/2020/day/7
/// Difficulty: m
/// Tags: dfs
/// Remarks:
module Year2020.Day07

open System
open Common.Helpers
open Common.Types

[<AutoOpen>]
module Graph =
    type Graph = Map<string, Map<string, int>>

    let dfs (source: string) (graph: Graph) =
        let visitedList: ResizeArray<string list> = ResizeArray()

        let rec dfsUtil (currentNode: string) (visited: string list) =
            let visited = currentNode :: visited

            if graph.ContainsKey currentNode then
                for node in graph[currentNode].Keys do
                    if not (List.contains node visited) then
                        dfsUtil node visited

            visitedList.Add <| List.rev visited

        dfsUtil source List.empty

        visitedList.ToArray()

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
        |> Map.ofSeq

    let parseBag (bagInfo: string) =
        match bagInfo.Split("bags contain", StringSplitOptions.TrimEntries) with
        | [| source; rest |] -> (source.Replace(" ", ""), getBags rest)
        | _ -> unreachable ()


    let transposedGraph (graph: Map<string, Map<string, int>>) =
        (Map.empty, graph)
        ||> Seq.fold (fun state edges ->
            let target, vertexMap = edges.Key, Map.toSeq edges.Value

            (state, vertexMap)
            ||> Seq.fold (fun newMap (vertex, weight) ->
                newMap
                |> Map.change vertex (fun value ->
                    match value with
                    | Some value -> Some(value |> (Map.add target weight))
                    | None -> Some(Map.ofSeq [ (target, weight) ]))))

    let parse = Seq.map parseBag >> Map.ofSeq

let solvePart1 graph =
    let paths =
        graph
        |> transposedGraph
        |> dfs "shinygold"
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.length

    paths - 1 // Remove the self bag

let solvePart2 graph =
    let bagsContained =
        graph
        |> dfs "shinygold"
        |> Seq.map (fun path ->
            let edges = Seq.pairwise path
            (1, edges) ||> Seq.fold (fun state (src, dst) -> state * graph[src][dst]))
        |> Seq.sum

    bagsContained - 1 // Remove the singleton node

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
