﻿/// Year 2015/1 - Not Quite Lisp
/// Link: https://adventofcode.com/2015/day/1
/// Difficulty: xs
/// Tags: sequence
/// Remarks:
module Year2015.Day01

open Common.Helpers
open Common.Types

let parse =
    Seq.head
    >> Seq.map (function
        | '(' -> 1
        | ')' -> -1
        | _ -> unreachable ())

let solvePart1 = Seq.sum
let solvePart2 = Seq.scan (+) 0 >> Seq.findIndex ((=) -1)

let solve: string seq -> Solution =
    parse >> (fun x -> (solvePart1 x, solvePart2 x)) >> BothInt
