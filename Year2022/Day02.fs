/// Year 2022/2 - Rock Paper Scissors
/// Link: https://adventofcode.com/2022/day/2
/// Difficulty: xs
/// Tags: tabular typed
/// Remarks: Refactor more
module Year2022.Day02

open Common.Helpers
open Common.Types

let parse: string seq -> (string * string) seq =
    Seq.map (_.Split(" ") >> (fun pair -> (pair[0], pair[1])))

type Item =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Win
    | Lose
    | Draw

let score (item, outcome) =
    let itemScore =
        match item with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let outcomeScore =
        match outcome with
        | Win -> 6
        | Lose -> 0
        | Draw -> 3

    itemScore + outcomeScore

let theirPick =
    function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> unreachable ()

let totalScore strategy play =
    Seq.map (strategy >> play >> score) >> Seq.sum

let solvePart1 =
    let myInstruction =
        function
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors
        | _ -> unreachable ()

    let play: Item * Item -> Item * Outcome =
        function
        | Rock, Rock -> (Rock, Draw)
        | Rock, Paper -> (Paper, Win)
        | Rock, Scissors -> (Scissors, Lose)
        | Paper, Rock -> (Rock, Lose)
        | Paper, Paper -> (Paper, Draw)
        | Paper, Scissors -> (Scissors, Win)
        | Scissors, Rock -> (Rock, Win)
        | Scissors, Paper -> (Paper, Lose)
        | Scissors, Scissors -> (Scissors, Draw)

    totalScore (fun (a, b) -> (theirPick a, myInstruction b)) play

let solvePart2 =
    let myInstruction =
        function
        | "X" -> Lose
        | "Y" -> Draw
        | "Z" -> Win
        | _ -> unreachable ()

    let play: Item * Outcome -> Item * Outcome =
        function
        | Rock, Win -> (Paper, Win)
        | Rock, Lose -> (Scissors, Lose)
        | Rock, Draw -> (Rock, Draw)
        | Paper, Win -> (Scissors, Win)
        | Paper, Lose -> (Rock, Lose)
        | Paper, Draw -> (Paper, Draw)
        | Scissors, Win -> (Rock, Win)
        | Scissors, Lose -> (Paper, Lose)
        | Scissors, Draw -> (Scissors, Draw)

    totalScore (fun (a, b) -> (theirPick a, myInstruction b)) play

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
