/// Year 2017/5 - A Maze of Twisty Trampolines, All Alike
/// Link: https://adventofcode.com/2017/day/5
/// Difficulty: xs
/// Tags: array
/// Remarks:
module Year2017.Day05

open Common.Types

let parse = Seq.map int >> Array.ofSeq

[<TailCall>]
let solvePart1 (input: int[]) =
    let arr = Array.copy input

    let rec jump idx steps =
        match idx with
        | crossed when crossed >= arr.Length -> steps
        | idx ->
            let offset = arr[idx]
            arr[idx] <- offset + 1
            jump (idx + offset) (steps + 1)

    jump 0 0

[<TailCall>]
let solvePart2 (input: int[]) =
    let arr = Array.copy input

    let rec jump idx steps =
        match idx with
        | crossed when crossed >= arr.Length -> steps
        | idx ->
            match arr[idx] with
            | offset when offset >= 3 ->
                arr[idx] <- offset - 1
                jump (idx + offset) (steps + 1)
            | offset ->
                arr[idx] <- offset + 1
                jump (idx + offset) (steps + 1)

    jump 0 0

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
