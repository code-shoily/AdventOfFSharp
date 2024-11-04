/// Year 2016/3 - Squares with Three Sides
/// Link: https://adventofcode.com/2016/day/3
/// Difficulty: xs
/// Tags: geometry
/// Remarks:
module Year2016.Day03

open Common.Helpers
open Common.Types

let parse =
    let sides: string -> int[] =
        _.Trim().Split(" ") >> Array.filter ((<>) "") >> Array.map int

    Seq.map sides >> Array.ofSeq

let isTriangle =
    function
    | [| a; b; c |] -> a + b > c && b + c > a && c + a > b
    | _ -> unreachable ()

let countTriangles = Array.filter isTriangle >> Array.length
let solvePart1 = countTriangles

let solvePart2 =
    Array.transpose >> Array.concat >> Array.chunkBySize 3 >> countTriangles

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
