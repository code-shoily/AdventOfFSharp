/// Year 2015/3 - Perfectly Spherical Houses in a Vacuum
/// Link: https://adventofcode.com/2015/day/3
/// Difficulty: xs
/// Tags: set direction
/// Remarks:
module Year2015.Day03

open Common.Helpers
open Common.Types

type Direction =
    | Up
    | Down
    | Left
    | Right

    static member fromChar =
        function
        | 'v' -> Down
        | '>' -> Right
        | '<' -> Left
        | '^' -> Up
        | _ -> unreachable ()

type House =
    { x: int
      y: int }

    member this.move =
        function
        | Up -> { x = this.x; y = this.y + 1 }
        | Down -> { x = this.x; y = this.y - 1 }
        | Left -> { x = this.x - 1; y = this.y }
        | Right -> { x = this.x + 1; y = this.y }

let housesVisited (directions: Direction list) =
    let origin = { x = 0; y = 0 }

    ((origin, set [ origin ]), directions)
    ||> Seq.fold (fun (lastHouse, houses) direction ->
        let nextHouse = lastHouse.move direction
        (nextHouse, houses.Add(nextHouse)))
    |> snd


let parse = Seq.exactlyOne >> Seq.map Direction.fromChar >> List.ofSeq

let solvePart1 = housesVisited >> Seq.length

let solvePart2 =
    let divisionOfLabour =
        List.indexed
        >> List.groupBy (fun (idx, _) -> idx % 2 = 0)
        >> List.map (snd >> List.map snd)

    divisionOfLabour
    >> List.map housesVisited
    >> List.reduce Set.union
    >> Seq.length

let solve (rawInput: string seq) =
    let input = parse rawInput

    BothInt(solvePart1 input, solvePart2 input)
