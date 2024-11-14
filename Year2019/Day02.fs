/// Year 2019/2 - 1202 Program Alarm
/// Link: https://adventofcode.com/2019/day/2
/// Difficulty: s
/// Tags: array simulation
/// Remarks:
module Year2019.Day02

open Common.Helpers
open Common.Types

let parse (input: string seq) =
    input |> Seq.exactlyOne |> _.Split(",") |> Seq.map int |> Array.ofSeq

let runCommand (noun, verb) (code: int[]) =
    let operate pos op =
        code[code[pos + 3]] <- op code[code[pos + 1]] code[code[pos + 2]]

    let rec doRunCommand start =
        match code[start] with
        | 99 -> code[0]
        | 1 ->
            operate start (+)
            doRunCommand (start + 4)
        | 2 ->
            operate start (*)
            doRunCommand (start + 4)
        | _ -> unreachable ()

    code[1] <- noun
    code[2] <- verb
    doRunCommand 0

let solvePart1 = runCommand (12, 2)

let solvePart2 input =
    seq { 0..99 }
    |> Seq.allPairs (seq { 0..99 })
    |> Seq.map (fun (i, j) ->
        match runCommand (i, j) (Array.copy input) with
        | 19690720 -> Some(i * 100 + j)
        | _ -> None)
    |> Seq.pick id

let solve (rawInput: string seq) =
    BothInt(solvePart1 <| parse rawInput, solvePart2 <| parse rawInput)
