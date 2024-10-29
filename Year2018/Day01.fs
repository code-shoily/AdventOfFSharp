/// Year 2018/1 - Chronal Calibration
/// Link: https://adventofcode.com/2018/day/1
/// Difficulty: xs
/// Tags: scan
/// Remarks: -
module Year2018.Day01

open Common.Types

let rec cycle sequence =
    seq {
        yield! sequence
        yield! cycle sequence
    }

let solvePart1: int seq -> int = Seq.sum

let solvePart2: int seq -> int =
    cycle
    >> Seq.scan
        (fun (visits: Set<int>, frequency: int, _) x ->

            let newFrequency = frequency + x

            match visits |> Set.contains newFrequency with
            | true -> visits, 0, Some newFrequency
            | false -> Set.add newFrequency visits, newFrequency, None)
        (set [], 0, None)
    >> Seq.map (fun (_, _, frequency) -> frequency)
    >> Seq.pick id

let solve (rawInput: string seq) =
    let input = rawInput |> Seq.map int
    BothInt(solvePart1 input, solvePart2 input)
