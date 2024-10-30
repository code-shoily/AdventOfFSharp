/// Year 2021/1 - Sonar Sweep
/// Link: https://adventofcode.com/2021/day/1
/// Difficulty: xs
/// Tags: sliding-window
/// Remarks:
module Year2021.Day01

open Common.Types

let measurementWindow size =
    Seq.windowed size
    >> Seq.filter (fun arr -> (Array.head arr) < (Array.last arr))
    >> Seq.length

let solvePart1 = measurementWindow 2
let solvePart2 = measurementWindow 4

let solve (rawInput: string seq) =
    let input = rawInput |> Seq.map (string >> int)

    BothInt(solvePart1 input, solvePart2 input)
