/// Year 2016/6 - Signals and Noise
/// Link: https://adventofcode.com/2016/day/6
/// Difficulty: xs
/// Tags: transpose frequency
/// Remarks:
module Year2016.Day06

open System
open Common.Types

let parse = Seq.transpose

let solvePart1: char seq seq -> string =
    let mostFrequent = Seq.groupBy id >> Seq.maxBy (snd >> Seq.length) >> fst
    Seq.map mostFrequent >> String.Concat

let solvePart2: char seq seq -> string =
    let leastFrequent = Seq.groupBy id >> Seq.minBy (snd >> Seq.length) >> fst
    Seq.map leastFrequent >> String.Concat

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothString(solvePart1 input, solvePart2 input)
