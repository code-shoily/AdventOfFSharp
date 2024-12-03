/// Year 2024/3 - Mull It Over
/// Link: https://adventofcode.com/2024/day/3
/// Difficulty: xs
/// Tags: regex fsm
/// Remarks:
module Year2024.Day03

open System.Text.RegularExpressions
open Common.Types

let multiply: string -> int =
    _.Replace("mul(", "")
    >> _.Replace(")", "")
    >> _.Split(",")
    >> Array.map int
    >> Array.reduce (*)

let solvePart1 =
    Seq.sumBy (
        Regex(@"mul\(\d+,\d+\)").Matches
        >> Seq.sumBy (_.Groups >> Seq.map _.Value >> Seq.map multiply >> Seq.sum)
    )

let solvePart2 input =
    let regex = Regex(@"(?>mul\(\d+,\d+\))|(?>do\(\))|(?>don't\(\))")

    let parsedInstructions =
        Seq.collect (regex.Matches >> Seq.collect (_.Groups >> Seq.map _.Value)) input

    ((true, 0), parsedInstructions)
    ||> Seq.fold (fun (shouldContinue, total) instruction ->
        match (shouldContinue, instruction) with
        | _, "do()" -> (true, total)
        | _, "don't()" -> (false, total)
        | true, _ -> (true, total + multiply instruction)
        | false, _ -> (false, total))
    |> snd

let solve (rawInput: string seq) =
    BothInt(solvePart1 rawInput, solvePart2 rawInput)
