/// Year 2017/3 - Spiral Memory
/// Link: https://adventofcode.com/2017/day/3
/// Difficulty: l
/// Tags: spiral
/// Remarks: Revisit this.
module Year2017.Day03

open System
open Common.Types

[<AutoOpen>]
module Arithmetic =
    let modulus a b = (a % b + b) % b

    let floorDiv a b =
        (a / b - Convert.ToInt32(((a < 0) <> (b < 0)) && (a % b <> 0)))

let parse = Seq.exactlyOne >> int

let getPosition (idx: int) =
    let radius = ((float idx - 1.0) |> sqrt |> int |> (+) 1) / 2
    let diameter = 2 * radius - 1
    let i = idx - (pown diameter 2) - 1

    match (i < diameter, i < 2 * diameter + 2, i < 3 * diameter + 2) with
    | true, _, _ -> radius, i - radius + 1
    | _, true, _ -> radius - i + diameter, radius
    | _, _, true -> -radius, radius - i - 1 + 2 * diameter + 2
    | _ -> i - radius - 3 * diameter - 2, -radius

let solvePart1 (input: int) =
    let x, y = getPosition input
    (abs x + abs y)

let solvePart2 (input: int) =
    let rec allocate i (cache: Map<int * int, int>) =
        function
        | result when result > input -> result
        | _ ->
            let x, y = getPosition i

            let value =
                [ -3 .. 5 ]
                |> Seq.map (fun n ->
                    let xx, yy = (x - 1 + modulus n 3, y + floorDiv n 3)

                    if cache |> Map.containsKey (xx, yy) then
                        cache[xx, yy]
                    else
                        0)
                |> Seq.sum

            allocate (i + 1) (cache |> Map.add (x, y) value) value

    allocate 2 (Map [ ((0, 0), 1) ]) 1

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
