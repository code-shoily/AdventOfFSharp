/// Year 2019/3 - Crossed Wires
/// Link: https://adventofcode.com/2019/day/3
/// Difficulty: m
/// Tags: set geometry
/// Remarks:
module Year2019.Day03

open Common.Helpers
open Common.Types

[<AutoOpen>]
module Geometry =
    type Direction =
        | Up of int
        | Down of int
        | Left of int
        | Right of int

        member this.Steps =
            match this with
            | Up x -> x
            | Down x -> x
            | Left x -> x
            | Right x -> x

    let drawLineFrom (x, y) =
        function
        | Up steps as up -> (x, y + 1), (x, y + steps), up
        | Down steps as down -> (x, y - 1), (x, y - steps), down
        | Left steps as left -> (x - 1, y), (x - steps, y), left
        | Right steps as right -> (x + 1, y), (x + steps, y), right

    let areIntersectingLines: int * int -> (int * int) * (int * int) -> bool =
        fun (x, y) ->
            function
            | (xi, yi), (xf, yf) when xi = xf -> y >= min yi yf && y <= max yi yf
            | (xi, yi), (xf, yf) when yi = yf -> x >= min xi xf && x <= max xi xf
            | _ -> false

    let positionInLine (x, y) =
        function
        | (xi, yi), (xf, yf) when xi = xf ->
            if x = xi && (y >= min yi yf && y <= max yi yf) then
                Some(abs (y - yi))
            else
                None
        | (xi, yi), (xf, yf) when yi = yf ->
            if y = yi && (x >= min xi xf && x <= max xi xf) then
                Some(abs (x - xi))
            else
                None
        | _ -> None

    let rec intersection
        : ((int * int) * (int * int) * Direction) * ((int * int) * (int * int) * Direction) -> (int * int) option =
        function
        | ((xvi, yvi), (xvf, yvf), vertical), ((xhi, yhi), (xhf, yhf), horizontal) when
            (vertical.IsUp || vertical.IsDown) || (horizontal.IsLeft || horizontal.IsRight)
            ->
            let isOnLeft = ((xvi, yvi), (xvf, yvf)) |> areIntersectingLines (xvi, yhf)
            let isOnRight = ((xhi, yhi), (xhf, yhf)) |> areIntersectingLines (xvi, yhf)
            if isOnLeft && isOnRight then Some((xvi, yhf)) else None

        | horizontal, vertical -> intersection (vertical, horizontal)

    let manhattan (x, y) = abs x + abs y

module InputParser =
    type WireConnections =
        { LeftSegments: ((int * int) * (int * int) * Direction) list
          RightSegments: ((int * int) * (int * int) * Direction) list }

        member this.intersectionOfBothWires =
            let partitionByOrientationVH (segment: ('a * 'a * Direction) list) =
                segment |> List.partition (fun (_, _, dir) -> dir.IsUp || dir.IsDown)

            let leftVertical, leftHorizontal = partitionByOrientationVH this.LeftSegments
            let rightVertical, rightHorizontal = partitionByOrientationVH this.RightSegments

            let findAllIntersections pair =
                pair ||> List.allPairs |> List.choose intersection

            [ (leftVertical, rightHorizontal) |> findAllIntersections
              (leftHorizontal, rightVertical) |> findAllIntersections ]
            |> List.concat

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

    let createWireConnections leftDirs rightDirs =
        let getSegments (dirs: Direction[]) =
            ({| Point = (0, 0); Segments = [] |}, dirs)
            ||> Seq.fold (fun state direction ->
                let _, endPoint, _ as data = drawLineFrom state.Point direction

                {| state with
                    Point = endPoint
                    Segments = data :: state.Segments |})
            |> _.Segments
            |> List.rev

        { LeftSegments = getSegments leftDirs
          RightSegments = getSegments rightDirs }

    let parse (rawInput: string seq) =
        rawInput |> getWires ||> createWireConnections


let solvePart1 = List.map manhattan >> List.min

let solvePart2 (wires: InputParser.WireConnections) (common: (int * int) list) =
    let findLeastSteps (wireSegments: ((int * int) * (int * int) * Direction) seq) =
        common
        |> Seq.map (fun (x, y) ->
            ({| Found = None; Steps = 1 |}, wireSegments)
            ||> Seq.scan (fun state (lineFrom, lineTo, direction) ->
                match (lineFrom, lineTo) |> positionInLine (x, y) with
                | Some(index) ->
                    {| state with
                        Found = Some(index + state.Steps) |}
                | None ->
                    {| state with
                        Steps = direction.Steps + state.Steps |})
            |> Seq.pick _.Found)
        |> List.ofSeq

    (findLeastSteps wires.LeftSegments, findLeastSteps wires.RightSegments)
    ||> List.zip
    |> List.minBy (fun pair -> pair ||> (+))
    ||> (+)

let solve (rawInput: string seq) =
    let wires = InputParser.parse rawInput
    let common = wires.intersectionOfBothWires
    BothInt(solvePart1 common, solvePart2 wires common)
