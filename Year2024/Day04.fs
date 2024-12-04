/// Year 2024/4 - Cares Search
/// Link: https://adventofcode.com/2024/day/4
/// Difficulty: s
/// Tags: matrix
/// Remarks:
module Year2024.Day04

open System
open Common.Types

type Matrix = char[][]

let withinRange size (v, h) =
    v >= 0 && v < size && h >= 0 && h < size

let findConnectedString (wordSearch: Matrix) =
    Seq.map (fun (v, h) -> wordSearch[v][h]) >> String.Concat

let solvePart1 (wordSearch: Matrix) =
    let size = wordSearch.Length

    let getAllDirections (v, h) =
        [ [ (v, h); (v, h + 1); (v, h + 2); (v, h + 3) ] // Horizontal
          [ (v, h); (v + 1, h); (v + 2, h); (v + 3, h) ] // Vertical
          [ (v, h); (v + 1, h + 1); (v + 2, h + 2); (v + 3, h + 3) ]
          [ (v, h); (v - 1, h + 1); (v - 2, h + 2); (v - 3, h + 3) ] ] // Diagonal
        |> List.filter (List.forall <| withinRange size)

    let foundXMAS =
        getAllDirections
        >> Seq.map (findConnectedString wordSearch)
        >> Seq.filter (fun found -> found = "XMAS" || found = "SAMX")


    Seq.allPairs [ 0 .. size - 1 ] [ 0 .. size - 1 ]
    |> Seq.collect foundXMAS
    |> Seq.length

let solvePart2 (wordSearch: Matrix) =
    let size = wordSearch.Length

    let getXVals (v, h) =
        let tl = (v - 1, h - 1)
        let br = (v + 1, h + 1)
        let tr = (v - 1, h + 1)
        let bl = (v + 1, h - 1)

        let validCorners = [ tl; br; tr; bl ] |> List.forall (withinRange size)

        if wordSearch[v][h] = 'A' && validCorners then
            let tlbr = [ tl; br ] |> findConnectedString wordSearch
            let trbl = [ tr; bl ] |> findConnectedString wordSearch

            (tlbr = "MS" || tlbr = "SM") && (trbl = "MS" || trbl = "SM")
        else
            false

    List.allPairs [ 0 .. size - 1 ] [ 0 .. size - 1 ]
    |> List.filter getXVals
    |> List.length

let rec solve (rawInput: string seq) =
    let input = rawInput |> Array.ofSeq |> Array.map Array.ofSeq
    BothInt(solvePart1 input, solvePart2 input)
