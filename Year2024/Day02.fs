/// Year 2024/2 - Red-Nosed Reports
/// Link: https://adventofcode.com/2024/day/2
/// Difficulty: xs
/// Tags: array brute-force
/// Remarks:
module Year2024.Day02

open Common.Types

let parse (rawInput: string seq) =
    let parseLevels: string -> int[] = _.Split(" ") >> Array.map int
    rawInput |> Seq.map parseLevels

let isSafe levels =
    let withinRange value = 1 <= value && value <= 3

    let growthBy progression =
        Array.pairwise >> Array.map (progression >> withinRange)

    let allIncrementing = growthBy (fun (sm, lg) -> lg - sm) levels |> Seq.forall id
    let allDecrementing = growthBy (fun (lg, sm) -> lg - sm) levels |> Seq.forall id

    allDecrementing || allIncrementing

let isValidAfterRemoval levels =
    Array.indexed levels
    |> Seq.tryFind (fun (idx, _) -> levels |> Array.removeAt idx |> isSafe)

let solvePart1 = Seq.filter isSafe >> Seq.length
let solvePart2 = Seq.choose isValidAfterRemoval >> Seq.length

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
