/// Year 2021/2 - Dive!
/// Link: https://adventofcode.com/2021/day/2
/// Difficulty: xs
/// Tags: navigation
/// Remarks: Maybe refactor into Objects?
module Year2021.Day02

open Common.Helpers
open Common.Types

type Command =
    | Forward of int
    | Down of int
    | Up of int

type Submarine =
    { depth: int
      horizontalPosition: int
      aim: int }

    member this.mult = this.depth * this.horizontalPosition

    static member init =
        { depth = 0
          horizontalPosition = 0
          aim = 0 }

let parse =
    let parseCommandLine (s: string) : Command =
        match s.Split " " with
        | [| "forward"; steps |] -> Forward(int steps)
        | [| "down"; steps |] -> Down(int steps)
        | [| "up"; steps |] -> Up(int steps)
        | _ -> unreachable ()

    Seq.map parseCommandLine

let solvePart1 commands =
    let commandReducer
        ({ depth = depth
           horizontalPosition = horizontalPosition
           aim = _ } as submarine)
        (cmd: Command)
        : Submarine =
        match cmd with
        | Forward steps ->
            { submarine with
                horizontalPosition = horizontalPosition + steps }
        | Down steps -> { submarine with depth = depth + steps }
        | Up steps -> { submarine with depth = depth - steps }

    (Submarine.init, commands) ||> Seq.fold commandReducer |> _.mult

let solvePart2 commands =
    let commandReducer
        ({ depth = depth
           horizontalPosition = horizontalPosition
           aim = aim } as submarine)
        (cmd: Command)
        : Submarine =
        match cmd with
        | Forward steps ->
            { submarine with
                horizontalPosition = horizontalPosition + steps
                depth = depth + steps * aim }
        | Down steps -> { submarine with aim = aim + steps }
        | Up steps -> { submarine with aim = aim - steps }

    (Submarine.init, commands) ||> Seq.fold commandReducer |> _.mult

let solve (rawInput: string seq) =
    let input = parse rawInput

    BothInt(solvePart1 input, solvePart2 input)
