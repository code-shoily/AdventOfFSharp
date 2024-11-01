/// Year 2016/1 - No Time for a Taxicab
/// Link: https://adventofcode.com/2016/day/1
/// Difficulty: s
/// Tags: navigation
/// Remarks: -
module Year2016.Day01

open Common.Helpers
open Common.Types

type Instruction =
    | Left of int
    | Right of int

type Position = int * int

type Facing =
    | North of Position
    | South of Position
    | East of Position
    | West of Position

let (|WithPosition|) facing =
    match facing with
    | North position -> position
    | West position -> position
    | South position -> position
    | East position -> position

let manhattanDistance (WithPosition(x, y)) = (abs x) + (abs y)

let parse (input: string seq) =
    let asInstruction instruction =
        let (|Prefix|_|) (lr: string) (s: string) =
            match s.StartsWith(lr) with
            | true -> Some(s.Substring(lr.Length))
            | false -> None

        match instruction with
        | Prefix "L" rest -> Left(int rest)
        | Prefix "R" rest -> Right(int rest)
        | _ -> unreachable ()

    input
    |> Seq.exactlyOne
    |> (fun (input: string) -> input.Split(", "))
    |> Seq.map asInstruction

let solvePart1: string seq -> int =
    let move facing =
        function
        | Left steps ->
            match facing with
            | North(x, y) -> West(x - steps, y)
            | West(x, y) -> South(x, y - steps)
            | South(x, y) -> East(x + steps, y)
            | East(x, y) -> North(x, y + steps)
        | Right steps ->
            match facing with
            | North(x, y) -> East(x + steps, y)
            | West(x, y) -> North(x, y + steps)
            | South(x, y) -> West(x - steps, y)
            | East(x, y) -> South(x, y - steps)

    parse >> Seq.fold move (North(0, 0)) >> manhattanDistance

let solvePart2: string seq -> int =
    let move facing =
        function
        | Left steps ->
            match facing with
            | North(x, y) -> [ for i in [ 1..steps ] -> West(x - i, y) ]
            | West(x, y) -> [ for i in [ 1..steps ] -> South(x, y - i) ]
            | South(x, y) -> [ for i in [ 1..steps ] -> East(x + i, y) ]
            | East(x, y) -> [ for i in [ 1..steps ] -> North(x, y + i) ]
        | Right steps ->
            match facing with
            | North(x, y) -> [ for i in [ 1..steps ] -> East(x + i, y) ]
            | West(x, y) -> [ for i in [ 1..steps ] -> North(x, y + i) ]
            | South(x, y) -> [ for i in [ 1..steps ] -> West(x - i, y) ]
            | East(x, y) -> [ for i in [ 1..steps ] -> South(x, y - i) ]

    parse
    >> Seq.fold
        (fun facing instruction ->
            let fst = facing |> List.rev |> List.head
            facing @ (move fst instruction))
        (List.singleton (North(0, 0)))
    >> Seq.scan
        (fun (visits: Position Set, _) facing ->
            let (WithPosition point) = facing

            if visits.Contains point then
                (visits, Some facing)
            else
                (visits + Set [ point ], None))
        (set [], None)
    >> Seq.map snd
    >> Seq.pick id
    >> manhattanDistance

let solve (input: string seq) : Solution =
    BothInt(solvePart1 input, solvePart2 input)
