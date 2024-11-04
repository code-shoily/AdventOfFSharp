/// Year 2016/2 - Bathroom Security
/// Link: https://adventofcode.com/2016/day/2
/// Difficulty: xs
/// Tags: navigation
/// Remarks:
module Year2016.Day02

open Common.Types
open Common.Helpers
open System

type Direction =
    | Up
    | Down
    | Left
    | Right

let toDirection =
    function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | _ -> unreachable ()

let parse = Seq.map (Seq.map toDirection)

let getValidCode (dialPad: string[][]) (x, y) direction =
    let newX, newY =
        match direction with
        | Up -> x - 1, y
        | Down -> x + 1, y
        | Left -> x, y - 1
        | Right -> x, y + 1

    if dialPad[newX][newY] <> " " then (newX, newY) else (x, y)

let crackKeyCode dialPad startingPoint directions =
    ((startingPoint, []), directions)
    ||> Seq.fold (fun (rowPos, res) line ->
        let thisPos = (rowPos, line) ||> Seq.fold (getValidCode dialPad)

        (thisPos, thisPos :: res))
    |> snd
    |> Seq.map (fun (x, y) -> dialPad[x][y])
    |> Seq.rev
    |> String.Concat

let solvePart1 =
    let dialPad =
        [| [| " "; " "; " "; " "; " " |]
           [| " "; "1"; "2"; "3"; " " |]
           [| " "; "4"; "5"; "6"; " " |]
           [| " "; "7"; "8"; "9"; " " |]
           [| " "; " "; " "; " "; " " |] |]

    (2, 2) |> crackKeyCode dialPad

let solvePart2 =
    let dialPad =
        [| [| " "; " "; " "; " "; " "; " "; " " |]
           [| " "; " "; " "; "1"; " "; " "; " " |]
           [| " "; " "; "2"; "3"; "4"; " "; " " |]
           [| " "; "5"; "6"; "7"; "8"; "9"; " " |]
           [| " "; " "; "A"; "B"; "C"; " "; " " |]
           [| " "; " "; " "; "D"; " "; " "; " " |]
           [| " "; " "; " "; " "; " "; " "; " " |] |]

    (3, 1) |> crackKeyCode dialPad

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothString(solvePart1 input, solvePart2 input)
