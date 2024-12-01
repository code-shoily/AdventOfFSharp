/// Year 2017/7 - ???
/// Link: https://adventofcode.com/2017/day/7
/// Difficulty:
/// Tags:
/// Remarks:

module Year2017.Day07

open System
open Common.Helpers
open Common.Types

[<Struct>]
type Disk =
    { weight: int
      parent: string option
      isLeaf: bool
      children: string[] }

let parse (rawInput: string seq) =
    let getInfo (disk: string) =
        match disk.Replace(")", "").Split("(", StringSplitOptions.TrimEntries) with
        | [| label; weight |] -> (label, int weight)
        | _ -> failwith "Error parsing getInfo"

    let diskInfo (input: string) =
        match input.Split(" -> ", StringSplitOptions.TrimEntries) with
        | [| leaf |] -> (getInfo leaf, [||])
        | [| parent; children |] -> (getInfo parent, children.Split(", ", StringSplitOptions.TrimEntries))
        | _ -> failwith "Error parsing DiskInfo"

    let parsedDiskData = rawInput |> Seq.map diskInfo

    let firstPass =
        (Map.empty, parsedDiskData)
        ||> Seq.fold (fun parents ((label, weight), children) ->
            parents
            |> Map.add
                label
                { weight = weight
                  parent = None
                  isLeaf = Array.isEmpty children
                  children = children })

    (firstPass, parsedDiskData)
    ||> Seq.fold (fun disks ((label, _), children) ->
        (disks, children)
        ||> Seq.fold (fun disks child ->
            let childWithParent =
                { disks[child] with
                    parent = Some label }

            disks |> Map.add child childWithParent))

let solvePart1 (input: Map<string, Disk>) =
    input |> Seq.filter (_.Value >> _.parent.IsNone) |> Seq.exactlyOne |> _.Key

let solvePart2 (input: Map<string, Disk>) = todo ()

let solve (rawInput: string seq) =
    let input = parse rawInput

    BothString(solvePart1 input, "TODO")
