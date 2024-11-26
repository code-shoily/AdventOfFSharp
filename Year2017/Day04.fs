/// Year 2017/4 - High-Entropy Passphrases
/// Link: https://adventofcode.com/2017/day/4
/// Difficulty: xs
/// Tags: anagram
/// Remarks:
module Year2017.Day04

open System
open Common.Types

let parse: string seq -> string[] seq = Seq.map _.Split(" ")

let countSets (by) =
    Array.map by >> Set.ofArray >> Set.count

let solvePart1 =
    let hasNoUnique (passphrases: string[]) =
        (passphrases |> countSets id) = Array.length passphrases

    Seq.filter hasNoUnique >> Seq.length

let solvePart2 =
    let hasNoAnagram (passphrases: string[]) =
        (passphrases |> countSets (Seq.sort >> String.Concat)) = Array.length passphrases

    Seq.filter hasNoAnagram >> Seq.length

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
