#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/IOUtils/bin/Debug/net9.0/IOUtils.dll"
#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/Common/bin/Debug/net9.0/Common.dll"

open AdventOfCode.FSharp.IOUtils

let getInput year day = readLines year day |> Option.get

let rawInput = getInput 2019 3

(* Copy Pasta *)
open Common.Helpers

[<AutoOpen>]
module Types =
    type Direction =
        | Up of int
        | Down of int
        | Left of int
        | Right of int

    type Orientation =
        | Vertical of int
        | Horizontal of int

    type WireConnections =
        { LeftSegments: ((int * int) * (int * int) * Orientation) list
          RightSegments: ((int * int) * (int * int) * Orientation) list }

    let drawLineFrom (x, y) =
        function
        | Up steps -> (x, y + 1), (x, y + steps), Vertical steps
        | Down steps -> (x, y - 1), (x, y - steps), Vertical steps
        | Left steps -> (x - 1, y), (x - steps, y), Horizontal steps
        | Right steps -> (x + 1, y), (x + steps, y), Horizontal steps

    let getWireDirections: string -> Direction[] =
        let getDirection (rep: string) =
            match (rep[0], rep[1..]) with
            | 'U', step -> Up(int step)
            | 'D', step -> Down(int step)
            | 'L', step -> Left(int step)
            | 'R', step -> Right(int step)
            | _ -> unreachable ()

        _.Split(",") >> Array.map getDirection

    let getWires: string seq -> Direction[] * Direction[] =
        List.ofSeq
        >> List.map getWireDirections
        >> function
            | [ a; b ] -> (a, b)
            | _ -> unreachable ()

[<AutoOpen>]
module Geometry =
    let containedIn (x, y) =
        function
        | (xi, yi), (xf, yf) when xi = xf -> y >= min yi yf && y <= max yi yf
        | (xi, yi), (xf, yf) when yi = yf -> x >= min xi xf && x <= max xi xf
        | _ -> false

    let rec intersection =
        function
        | ((xvi, yvi), (xvf, yvf), Vertical _), ((xhi, yhi), (xhf, yhf), Horizontal _) ->
            let isOnLeft = ((xvi, yvi), (xvf, yvf)) |> containedIn (xvi, yhf)
            let isOnRight = ((xhi, yhi), (xhf, yhf)) |> containedIn (xvi, yhf)
            if isOnLeft && isOnRight then Some((xvi, yhf)) else None

        | horizontal, vertical -> intersection (vertical, horizontal)

    let manhattan (x, y) = abs x + abs y


let intersectionOfBothWires left right =
    let partitionByOrientationVH =
        List.partition (function
            | _, _, Vertical _ -> true
            | _ -> false)

    let leftVertical, leftHorizontal = partitionByOrientationVH left
    let rightVertical, rightHorizontal = partitionByOrientationVH right

    let findAllIntersections pair =
        pair ||> List.allPairs |> List.choose intersection

    [ (leftVertical, rightHorizontal) |> findAllIntersections
      (leftHorizontal, rightVertical) |> findAllIntersections ]
    |> List.concat

let parse (rawInput: string seq) =
    let left, right = rawInput |> getWires

    let getSegments (dirs: Direction[]) =
        (((0, 0), []), dirs)
        ||> Seq.fold (fun (fromPoint, segments) direction ->
            let _, endPoint, _ as data = drawLineFrom fromPoint direction
            (endPoint, data :: segments))
        |> snd
        |> List.rev

    { LeftSegments = getSegments left
      RightSegments = getSegments right }


let solvePart1 (wires: WireConnections) =
    (wires.LeftSegments, wires.RightSegments)
    ||> intersectionOfBothWires
    |> List.map manhattan
    |> List.min

(* Experiments go here *)
let data = rawInput |> parse
solvePart1 data
