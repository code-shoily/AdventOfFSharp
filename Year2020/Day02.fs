/// Year 2020/2 - Password Philosophy
/// Link: https://adventofcode.com/2020/day/2
/// Difficulty: xs
/// Tags: string
/// Remarks:
module Year2020.Day02

open Common.Helpers
open Common.Types

type Policy =
    { lo: int
      hi: int
      ch: char
      password: string }


let parse =
    let createPolicy (line: string) =
        match line.Split " " with
        | [| range; ch; password |] ->
            let lo, hi =
                range.Split "-" |> (fun arr -> int <| Array.head arr, int <| Array.last arr)


            let ch = Seq.head ch

            { lo = lo
              hi = hi
              ch = ch
              password = password }
        | _ -> unreachable ()

    Seq.map createPolicy

let solvePart1 =
    let isValidV1
        { lo = lo
          hi = hi
          ch = ch
          password = password }
        =
        let frequency = password |> Seq.filter (fun c -> c = ch) |> Seq.length
        frequency >= lo && frequency <= hi

    Seq.filter isValidV1 >> Seq.length

let solvePart2 =
    let isValidV2
        { lo = lo
          hi = hi
          ch = ch
          password = password }
        =
        let loChar = password[lo - 1] = ch
        let hiChar = password[hi - 1] = ch

        (loChar <> hiChar) && (loChar || hiChar)

    Seq.filter isValidV2 >> Seq.length

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
