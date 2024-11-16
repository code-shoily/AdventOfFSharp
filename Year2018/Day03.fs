/// Year 2018/3 - No Matter How You Slice It
/// Link: https://adventofcode.com/2018/day/3
/// Difficulty: xs
/// Tags: fold geometry slightly-slow
/// Remarks: Refactor with intersection tree.
module Year2018.Day03

open System.Text.RegularExpressions
open Common.Helpers
open Common.Types

type Claim =
    { ID: int
      TopLeft: int * int
      BottomRight: int * int
      Area: int }

    member this.GetAllPoints =
        [ (snd this.TopLeft) .. (snd this.BottomRight) ]
        |> Seq.allPairs [ (fst this.TopLeft) .. (fst this.BottomRight) ]

    static member FromStatement =
        Regex("#(\d+) @ (\d+),(\d+): (\d+)x(\d+)").Match
        >> _.Groups
        >> _.Values
        >> Seq.map _.Value
        >> Seq.skip 1
        >> Seq.map int
        >> List.ofSeq
        >> function
            | [ id; x; y; w; h ] ->
                { ID = id
                  TopLeft = (x, y)
                  BottomRight = (x + w - 1, y + h - 1)
                  Area = w * h }
            | _ -> unreachable ()

let claimFabrics (table: Map<int * int, Set<Claim>>) (claim: Claim) =
    (table, claim.GetAllPoints)
    ||> Seq.fold (fun table key ->
        table
        |> Map.change key (function
            | None -> Some(set [ claim ])
            | Some(v) -> Some(v.Add(claim))))

let parse =
    Seq.toArray
    >> Array.Parallel.map Claim.FromStatement
    >> (Seq.fold claimFabrics Map.empty)

let solvePart1: Map<int * int, Set<Claim>> -> int =
    Map.filter (fun _ v -> v.Count <> 1) >> Seq.length

let solvePart2: Map<int * int, Set<Claim>> -> int =
    Map.toSeq
    >> Seq.filter (snd >> _.Count >> (=) 1)
    >> Seq.groupBy (snd >> Set.toSeq >> Seq.exactlyOne)
    >> Seq.pick (fun (claim, fabrics) ->
        if claim.Area = Seq.length fabrics then
            Some(claim.ID)
        else
            None)

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
