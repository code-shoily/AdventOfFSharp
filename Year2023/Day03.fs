/// Year 2023/3 - Gear Ratios
/// Link: https://adventofcode.com/2023/day/3
/// Difficulty: m
/// Tags: annoying set slow
/// Remarks: Need to refactor using parent table (disjoint set)
module Year2023.Day03

open System
open Common.Types

module Part =
    type State =
        { Count: int
          Parts: char list
          Positions: (int * int) list
          Collection: ((int * int) * int) seq }

        static member empty =
            { Count = 0
              Parts = List.empty
              Positions = List.empty
              Collection = Seq.empty }

    let onPartFound (rowIdx: int) (colIdx: int) (digit: char) (state: State) =
        { state with
            Count = colIdx
            Parts = digit :: state.Parts
            Positions = (rowIdx, colIdx) :: state.Positions }

    let onPartCollected (state: State) =
        if not state.Parts.IsEmpty then
            let part = state.Parts |> Seq.rev |> String.Concat |> int

            { state with
                Count = 0
                Parts = List.empty
                Positions = List.empty
                Collection =
                    seq {
                        yield! state.Collection

                        for pos in state.Positions do
                            yield (pos, part)
                    } }
        else
            state

    let build (lineIdx: int) (line: string) =
        (State.empty, Seq.indexed line)
        ||> Seq.fold (fun state (idx, value) ->
            if Char.IsDigit(value) then
                (value, state) ||> onPartFound lineIdx idx
            else
                state |> onPartCollected)
        |> onPartCollected
        |> _.Collection

module Schematic =
    let getAdjacentSet (x, y) =
        Set.ofList
        <| [ (x + 1, y)
             (x - 1, y)
             (x, y + 1)
             (x, y - 1)
             (x + 1, y + 1)
             (x - 1, y - 1)
             (x + 1, y - 1)
             (x - 1, y + 1) ]

    let buildTable (lineIdx: int) (line: string) =
        line
        |> Seq.mapi (fun idx ->
            function
            | '.' -> None
            | part when Char.IsDigit(part) -> None
            | gear -> Some(((lineIdx, idx), gear)))
        |> Seq.choose id

    let symbols table = table |> Map |> _.Keys

    let gears table =
        table |> Seq.filter (fun (_, v) -> v = '*') |> symbols

[<AutoOpen>]
module Geometry =
    let groupByRows lst =
        lst |> Seq.groupBy fst |> Seq.map (snd >> Seq.map snd)

    let countPoints ys =
        ({| Count = 1; Last = Seq.head ys |}, Seq.skip 1 ys)
        ||> Seq.fold (fun state current ->
            if current - state.Last = 1 then
                {| state with Last = current |}
            else
                {| state with
                    Count = state.Count + 1
                    Last = current |})
        |> _.Count

let solvePart1 (parts, symbolTable) =
    let connectedParts parts symbols =
        let gearEdges =
            symbols |> Seq.collect (Schematic.getAdjacentSet >> Set.toSeq) |> Set.ofSeq

        let partLocations = parts |> Map.keys |> Set.ofSeq

        Set.intersect gearEdges partLocations
        |> Seq.map (fun location -> (location, parts[location]))
        |> Seq.groupBy snd
        |> Seq.map (fun (part, locations) -> (part, locations |> Seq.sort |> Seq.map fst))

    symbolTable
    |> Schematic.symbols
    |> connectedParts parts
    |> Seq.map (fun (a, b) -> a * (groupByRows b |> Seq.map countPoints |> Seq.sum))
    |> Seq.sum

let solvePart2 (parts, symbolTable) =
    let gears = Schematic.gears symbolTable
    let partLocations = parts |> Map.keys |> Set.ofSeq

    let validGears =
        gears
        |> Seq.choose (fun gearLocation ->
            let connectedPoints =
                gearLocation
                |> Schematic.getAdjacentSet
                |> (Set.intersect partLocations)
                |> Set.toSeq
                |> groupByRows
                |> Seq.map countPoints
                |> Seq.sum

            if connectedPoints = 2 then Some(gearLocation) else None)

    validGears
    |> Seq.map (
        Schematic.getAdjacentSet
        >> Set.intersect partLocations
        >> Seq.map (fun point -> parts[point])
        >> Seq.distinct
        >> (fun x -> (Seq.head x * Seq.last x))
    )
    |> Seq.sum

let parse (rawInput: string seq) =
    let parts = rawInput |> Seq.mapi Part.build |> Seq.concat |> Map
    let symbolTable = rawInput |> Seq.mapi Schematic.buildTable |> Seq.concat
    parts, symbolTable

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
