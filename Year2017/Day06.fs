/// Year 2017/6 - Memory Reallocation
/// Link: https://adventofcode.com/2017/day/6
/// Difficulty: s
/// Tags: off-by-one mutable array
/// Remarks: Should I investigate a more functional solution to this?
module Year2017.Day06

open Common.Types

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
