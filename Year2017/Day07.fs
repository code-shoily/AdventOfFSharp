/// Year 2017/7 - Recursive Circus
/// Link: https://adventofcode.com/2017/day/7
/// Difficulty: l
/// Tags: bfs optimization
/// Remarks:

module Year2017.Day07

open System.Text.RegularExpressions
open Common.Helpers
open Common.Types

type Disk =
    { Weight: int
      Parent: string option
      Children: string[] }

    member this.IsLeaf = Array.isEmpty this.Children

    static member asTokenized(line: string) =
        let regex = Regex(@"(\S+) \((\d+)\)(?> -> ([a-z, ]+))?")

        match line |> regex.Match |> _.Groups |> Array.ofSeq |> Array.map _.Value with
        | [| _; parent; weight; "" |] -> ((parent, int weight), [||])
        | [| _; parent; weight; children |] -> ((parent, int weight), children.Split(", "))
        | _ -> unreachable ()

type Graph = Map<string, Disk>
type WeightedNode = string * int

module GraphUtil =
    let rec getWeight (disk: Disk) (graph: Graph) =
        if disk.IsLeaf then
            disk.Weight
        else
            disk.Weight
            + (disk.Children |> Seq.sumBy (fun child -> graph |> getWeight graph[child]))

    let getAllWeights (graph: Graph) =
        graph.Values
        |> Seq.collect (_.Children >> Seq.map (fun child -> (child, graph |> getWeight graph[child])))
        |> Map.ofSeq

    let diskToBeUpdated (pairs: WeightedNode[]) =
        let diskNodes = pairs |> Seq.map snd |> Seq.toList
        let delta = Seq.max diskNodes - Seq.min diskNodes
        let table = diskNodes |> List.countBy ((=) (Seq.min diskNodes)) |> Map.ofSeq

        match (table[true], table[false]) with
        | 1, 1 -> None // Only two children, can't be determined
        | 1, _ -> Some(true, delta) // Single value is decreasing
        | _, 1 -> Some(false, delta) // Single value is increasing
        | _ -> None // Can't be determined

    let getImbalancedNode root graph =
        let mutable isShrinking = None

        let weights = getAllWeights graph
        let weightGroups = ResizeArray<WeightedNode[]>()

        let hasDifferenceInWeight (ls: WeightedNode[]) =
            (ls |> Array.distinctBy snd |> Array.length) = 2

        let rec traverse (key: string) =
            let children =
                graph[key].Children |> Array.map (fun childKey -> (childKey, weights[childKey]))

            if children |> hasDifferenceInWeight then
                if isShrinking.IsNone then
                    isShrinking <- diskToBeUpdated children

                weightGroups.Add children

            for childKey in graph[key].Children do
                traverse childKey

        let getLastNode = Seq.last >> Array.distinctBy snd >> Array.sortBy snd

        traverse root

        match (isShrinking, getLastNode weightGroups) with
        | Some(false, diff), [| _; large |] -> (-1 * diff, fst large)
        | Some(true, diff), [| small; _ |] -> (diff, fst small)
        | _ -> unreachable ()

let parse (rawInput: string seq) =
    let diskTokens = rawInput |> Seq.map Disk.asTokenized

    let disksWithoutParents =
        (Map.empty, diskTokens)
        ||> Seq.fold (fun graph ((label, weight), children) ->
            graph
            |> Map.add
                label
                { Weight = weight
                  Parent = None
                  Children = children })

    (disksWithoutParents, diskTokens)
    ||> Seq.fold (fun disks ((label, _), children) ->
        (disks, children)
        ||> Seq.fold (fun disks child ->
            let childWithParent =
                { disks[child] with
                    Parent = Some label }

            disks |> Map.add child childWithParent))

let solvePart1 (input: Graph) =
    input |> Seq.filter (_.Value >> _.Parent.IsNone) |> Seq.exactlyOne |> _.Key

let solvePart2 graph root =
    let diff, label = graph |> GraphUtil.getImbalancedNode root
    graph[label].Weight + diff

let solve (rawInput: string seq) =
    let input = parse rawInput
    let root = solvePart1 input
    StringInt(root, solvePart2 input root)
