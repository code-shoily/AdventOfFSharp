/// Year 2024/8 - Resonant Collinearity
/// Link: https://adventofcode.com/2024/day/8
/// Difficulty: xs
/// Tags: slope-intercept geometry array2d
/// Remarks:
module Year2024.Day08

open Common.Types

type SlopeIntercept =
    { m: float
      c: float }

    member this.solves(x, y) = abs (y - (this.m * x + this.c)) < 0.001

    static member fromPoints (av: int, ah: int) (bv: int, bh: int) =
        let x, y, x', y' = float av, float ah, float bv, float bh
        let m = (y' - y) / (x' - x)
        { m = m; c = y - m * x }

let solvePart1 antennas vSize hSize =
    let isValidCoord (v, h) =
        0 <= v && v < vSize && 0 <= h && h < hSize

    let doubleDistance a b = a + (b - a) * 2

    let antinodes antennas =
        List.allPairs antennas antennas
        |> List.choose (function
            | a, b when a = b -> None
            | (av, ah), (bv, bh) -> Some(doubleDistance av bv, doubleDistance ah bh))

    antennas
    |> List.collect antinodes
    |> List.filter isValidCoord
    |> List.distinct
    |> List.length

let solvePart2 antenna vSize hSize =
    let isBiggerThan (av, ah) (bv, bh) = if av = bv then ah < bh else av < bv

    let getSameFrequencyAntennas (antennas: (int * int) list) =
        List.allPairs antennas antennas
        |> List.filter (function
            | a, b when b |> isBiggerThan a -> true
            | _ -> false)

    let equations =
        List.collect (
            getSameFrequencyAntennas
            >> List.map (fun pair -> pair ||> SlopeIntercept.fromPoints)
        )
        >> List.distinct

    List.allPairs [ 0 .. vSize - 1 ] [ 0 .. hSize - 1 ]
    |> List.map (fun (x, y) -> antenna |> equations |> List.tryFind _.solves(x, y))
    |> List.choose id
    |> List.length

let parse (rawInput: string seq) =
    let antennas =
        rawInput
        |> Seq.mapi (fun v -> Seq.mapi (fun h cell -> ((v, h), cell)))
        |> Seq.collect id
        |> Seq.filter (snd >> (=) '.' >> not)
        |> List.ofSeq
        |> List.groupBy snd
        |> List.map (snd >> List.map fst)

    antennas, Seq.length rawInput, rawInput |> Seq.head |> Seq.length

let solve (rawInput: string seq) =
    let _antennas, _vSize, _hSize as input = parse rawInput
    BothInt(input |||> solvePart1, input |||> solvePart2)
