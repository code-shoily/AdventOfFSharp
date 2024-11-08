/// Year 2019/3 - Crossed Wires
/// Link: https://adventofcode.com/2019/day/3
/// Difficulty: s
/// Tags: navigation2d set slow
/// Remarks: Refactor, use geometry
module Year2019.Day03

open Common.Helpers
open Common.Types

module Wire =
    type Direction =
        | Left of int
        | Right of int
        | Up of int
        | Down of int

    let step (x, y) =
        (function
        | Up steps ->
            [ for i in [ 1..steps ] do
                  (x, y + i) ]
        | Down steps ->
            [ for i in [ 1..steps ] do
                  (x, y - i) ]
        | Left steps ->
            [ for i in [ 1..steps ] do
                  (x - i, y) ]
        | Right steps ->
            [ for i in [ 1..steps ] do
                  (x + i, y) ])

    let draw (directions: Direction seq) =
        let wirePath =
            ([ (0, 0) ], directions)
            ||> Seq.fold (fun path -> step (Seq.last path) >> List.append path)

        ((0, Map.empty), wirePath)
        ||> Seq.fold (fun (idx, visits) point ->
            let updatedVisits =
                match visits |> Map.containsKey point with
                | true -> visits
                | false -> visits |> Map.add point idx

            (idx + 1, updatedVisits))
        |> snd
        |> Map.remove (0, 0)

    let parse (input: string seq) =
        let translate (wireRep: string) : Direction =
            match wireRep[0], wireRep[1..] with
            | 'R', steps -> Right(int steps)
            | 'L', steps -> Left(int steps)
            | 'U', steps -> Up(int steps)
            | 'D', steps -> Down(int steps)
            | _ -> unreachable ()

        let left, right =
            input
            |> Seq.map (_.Split(",") >> Seq.map translate)
            |> (fun x -> Seq.head x, Seq.last x)

        draw left, draw right

let solvePart1 = Seq.map (fun (a, b) -> abs a + abs b) >> Seq.min

let solvePart2 (leftPath: Map<int * int, int>) (rightPath: Map<int * int, int>) =
    Seq.map (fun point -> leftPath[point] + rightPath[point]) >> Seq.min

let solve (rawInput: string seq) =
    let wireLeft, wireRight = Wire.parse rawInput

    let intersections =
        (wireLeft, wireRight)
        ||> (fun a b -> Set.ofSeq a.Keys, Set.ofSeq b.Keys)
        ||> Set.intersect

    BothInt(intersections |> solvePart1, intersections |> solvePart2 wireLeft wireRight)
