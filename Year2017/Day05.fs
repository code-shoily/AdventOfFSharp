<<<<<<< HEAD
/// Year 2017/5 - A Maze of Twisty Trampolines, All Alike
/// Link: https://adventofcode.com/2017/day/5
/// Difficulty: xs
/// Tags: array
/// Remarks:
=======
/// Year 2017/5 - Memory Reallocation
/// Link: https://adventofcode.com/2017/day/5
/// Difficulty: s
/// Tags: off-by-1 array mutable
/// Remarks: Should investigate a mutable solution. Beware of off by 1 errors.
>>>>>>> 65d51d5 (Solve 2017/05)
module Year2017.Day05

open Common.Types

<<<<<<< HEAD
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
=======
let parse: string seq -> int[] = Seq.exactlyOne >> _.Split("\t") >> Array.map int

let rec distribute (arr: int[]) =
    let maxIdx = arr |> Array.indexed |> Array.maxBy snd |> fst
    let distributionCount = arr[maxIdx]

    let rec doDistribute currentIteration =
        let idx = currentIteration % (Array.length arr)

        function
        | 0 -> arr
        | distributionLeft ->
            arr[idx] <- arr[idx] + 1
            doDistribute (currentIteration + 1) (distributionLeft - 1)

    arr[maxIdx] <- arr[maxIdx] - distributionCount
    distributionCount |> doDistribute (maxIdx + 1)

let detectRepetition (input: int[]) =
    let asID: int[] -> string = Array.map string >> String.concat ","

    let rec findDistributions (distributions: Set<string>) =
        let newDistribution = (distribute input) |> asID

        if distributions |> Set.contains newDistribution then
            1 + Set.count distributions
        else
            findDistributions (distributions |> Set.add newDistribution)

    findDistributions Set.empty

let solve (rawInput: string seq) =
    let input = parse rawInput
    let solvePart1 = detectRepetition input
    let solvePart2 = detectRepetition input - 1

    BothInt(solvePart1, solvePart2)
>>>>>>> 65d51d5 (Solve 2017/05)
