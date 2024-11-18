/// Year 2023/3 - Gear Ratios
/// Link: https://adventofcode.com/2023/day/3
/// Difficulty: m
/// Tags: disjoint-set
/// Remarks:
module Year2023.Day03

open System
open Common.Types

type Input =
    { Parts: Map<int * int, int>
      SymbolTable: ((int * int) * char) seq
      Parents: Map<int * int, int * int> }

type PartState =
    { Count: int
      Origin: (int * int) option
      Parents: Map<int * int, int * int>
      Parts: char list
      Positions: (int * int) list
      Collection: ((int * int) * int) seq }

    static member empty =
        { Count = 0
          Origin = None
          Parents = Map.empty
          Parts = List.empty
          Positions = List.empty
          Collection = Seq.empty }

let onPartFound (rowIdx: int) (colIdx: int) (digit: char) (state: PartState) =
    let { Origin = origin
          Parents = parents
          Parts = parts
          Positions = positions } =
        state

    let newOrigin = origin |> Option.defaultValue (rowIdx, colIdx) |> Some

    { state with
        Count = colIdx
        Parents = parents |> Map.add (rowIdx, colIdx) newOrigin.Value
        Origin = newOrigin
        Parts = digit :: parts
        Positions = (rowIdx, colIdx) :: positions }

let onPartCollected (state: PartState) =
    let { Collection = collection
          Parts = parts
          Positions = positions } =
        state

    if not parts.IsEmpty then
        let part = parts |> Seq.rev |> String.Concat |> int

        { state with
            Count = 0
            Origin = None
            Parts = List.empty
            Positions = List.empty
            Collection =
                seq {
                    yield! collection

                    for pos in positions do
                        yield (pos, part)
                } }
    else
        state

let getPartsData (lineIdx: int) (line: string) =
    let metadata =
        (PartState.empty, Seq.indexed line)
        ||> Seq.fold (fun state (idx, value) ->
            if Char.IsDigit(value) then
                (value, state) ||> onPartFound lineIdx idx
            else
                state |> onPartCollected)
        |> onPartCollected

    metadata.Collection, metadata.Parents

let adjacencySetOf (x, y) =
    Set.ofList
    <| [ (x + 1, y)
         (x - 1, y)
         (x, y + 1)
         (x, y - 1)
         (x + 1, y + 1)
         (x - 1, y - 1)
         (x + 1, y - 1)
         (x - 1, y + 1) ]

let buildTable (lineIdx: int) =
    Seq.mapi (fun idx ->
        function
        | '.' -> None
        | part when Char.IsDigit(part) -> None
        | gear -> Some((lineIdx, idx), gear))
    >> Seq.choose id

let allSymbols = Seq.map fst
let gears = Seq.filter (snd >> (=) '*') >> allSymbols

let parse (rawInput: string seq) =
    let parts = rawInput |> Seq.mapi getPartsData

    { Parts = parts |> Seq.map fst |> Seq.concat |> Map
      SymbolTable = rawInput |> Seq.mapi buildTable |> Seq.concat
      Parents = parts |> Seq.map snd |> Seq.collect Map.toSeq |> Map }


let solvePart1
    { Parts = parts
      SymbolTable = symbolTable
      Parents = parents }
    =
    let gearEdges =
        symbolTable
        |> allSymbols
        |> Seq.collect (adjacencySetOf >> Set.toSeq)
        |> Set.ofSeq

    let partLocations = parts |> Map.keys |> Set.ofSeq

    Set.intersect gearEdges partLocations
    |> Seq.map (fun p -> parents[p])
    |> Seq.distinct
    |> Seq.map (fun p -> parts[p])
    |> Seq.sum

let solvePart2
    { Parts = parts
      SymbolTable = symbolTable
      Parents = parents }
    =
    gears symbolTable
    |> Seq.map (
        adjacencySetOf
        >> Set.intersect (parts |> Map.keys |> Set.ofSeq)
        >> Seq.map (fun p -> parents[p])
        >> Seq.distinct
        >> List.ofSeq
        >> (function
        | [ p1; p2 ] -> parts[p1] * parts[p2]
        | _ -> 0)
    )
    |> Seq.sum

let solve (rawInput: string seq) =
    let input = parse rawInput

    BothInt(solvePart1 input, solvePart2 input)
