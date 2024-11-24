#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/IOUtils/bin/Debug/net9.0/IOUtils.dll"
#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/Common/bin/Debug/net9.0/Common.dll"

open IOUtils

let getInput year day = readLines year day |> Option.get

open System
open Common.Types
open Common.Helpers

let rawInput = getInput 2020 6
(* Copy Pasta *)
(* Experiments go here *)
let parse = List.ofSeq >> paragraphs

let getQuestionsBy reducer =
    List.map (List.map Set.ofSeq >> List.reduce reducer >> Seq.length) >> List.sum

let solvePart1 = getQuestionsBy Set.union
let solvePart2 = getQuestionsBy Set.intersect

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)

rawInput |> solve
