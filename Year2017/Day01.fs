/// Year 2017/1 - Inverse Captcha
/// Link: https://adventofcode.com/2017/day/1
/// Difficulty: xs
/// Tags: array
/// Remarks: -
module Year2017.Day01

open Common.Types

let solvePart1 =
    (fun x -> Array.append x [| Array.head x |])
    >> Array.windowed 2
    >> Array.filter (fun arr -> arr[0] = arr[1])
    >> Array.map Array.head
    >> Array.sum

let solvePart2 =
    Array.splitInto 2
    >> (fun pair -> Seq.zip pair[0] pair[1])
    >> Seq.map (function
        | a, b when a = b -> a + b
        | _ -> 0)
    >> Seq.sum

let solve (rawInput: string seq) =
    let input = rawInput |> Seq.head |> Seq.map (string >> int) |> Array.ofSeq
    BothInt(solvePart1 input, solvePart2 input)
