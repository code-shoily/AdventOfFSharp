/// Year 2021/3 - Binary Diagnostic
/// Link: https://adventofcode.com/2021/day/3
/// Difficulty: xs
/// Tags: reduction
/// Remarks:
module Year2021.Day03

open System
open Common.Types

let parse = Seq.map (Seq.map (string >> int) >> Array.ofSeq) >> Array.ofSeq

type Diagnostics =
    { One: int
      Zero: int }

    member this.mostCommon = if this.Zero > this.One then 0 else 1
    member this.leastCommon = if this.Zero > this.One then 1 else 0


let diagnose =
    Array.transpose
    >> Array.map (Array.partition ((=) 0) >> fun (a, b) -> { Zero = a.Length; One = b.Length })

let binaryToInt (n: int array) = Convert.ToInt32(String.Concat n, 2)

let solvePart1 (input: int[][]) =
    let bitFrequencies = diagnose input

    let gamma = bitFrequencies |> Array.map (_.mostCommon)

    let epsilon = bitFrequencies |> Array.map (_.leastCommon)

    (binaryToInt gamma) * (binaryToInt epsilon)

type Scrubber =
    | O2
    | CO2

let solvePart2 (input: int[][]) =
    let rec scrubberRating n scrubberType (rows: int[][]) =
        if rows.Length = 1 then
            rows[0] |> binaryToInt
        else
            let diagnostics = (diagnose rows)[n]

            let scrubberBit =
                match scrubberType with
                | O2 -> diagnostics.mostCommon
                | CO2 -> diagnostics.leastCommon

            rows
            |> Array.filter (fun x -> x[n] = scrubberBit)
            |> scrubberRating (n + 1) scrubberType

    let o2Rating = scrubberRating 0 O2
    let co2Rating = scrubberRating 0 CO2

    (o2Rating input) * (co2Rating input)

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
