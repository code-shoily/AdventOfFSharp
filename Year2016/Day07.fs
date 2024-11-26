/// Year 2016/7 - Internet Protocol Version 7
/// Link: https://adventofcode.com/2016/day/7
/// Difficulty: xs
/// Tags: window
/// Remarks:
module Year2016.Day07

open System
open Common.Helpers
open Common.Types

module Helpers =
    let hasABBA (sequence: string) =
        sequence
        |> Seq.windowed 4
        |> Seq.exists (function
            | [| a; b; x; y |] -> a = y && b = x && a <> b
            | _ -> unreachable ())

    let getABAPair (sequence: string) =
        sequence
        |> Seq.windowed 3
        |> Seq.map (function
            | [| a; b; c |] ->
                if a = c && a <> b then
                    Some(String.Concat [ a; b; a ], String.Concat [ b; a; b ])
                else
                    None
            | _ -> unreachable ())
        |> Seq.choose id

type IPAddress =
    { Supernets: string seq
      Hypernets: string seq }

    member this.supportsTLS =
        let supernetsHaveABBA = this.Supernets |> Seq.exists Helpers.hasABBA
        let hypernetsHaveABBA = this.Hypernets |> Seq.exists Helpers.hasABBA

        supernetsHaveABBA && not hypernetsHaveABBA

    member this.supportsSSL =
        let supernetBABs =
            this.Supernets |> Seq.collect Helpers.getABAPair |> Seq.map snd |> Set.ofSeq

        let hypernetBABs =
            this.Hypernets
            |> Seq.collect Helpers.getABAPair
            |> Seq.map fst
            |> Set.ofSeq
            |> Set.intersect supernetBABs

        hypernetBABs.Count > 0

    static member fromLine(line: string) =
        ({| Outside = List.empty
            Inside = List.empty
            Current = List.empty |},
         line)
        ||> Seq.fold (fun state ch ->
            let outside, inside, current = state.Outside, state.Inside, state.Current

            match ch with
            | '[' ->
                {| state with
                    Outside = current :: outside
                    Current = List.empty |}
            | ']' ->
                {| state with
                    Inside = current :: inside
                    Current = List.empty |}
            | ch -> {| state with Current = ch :: current |})
        |> (fun seqState ->
            { Supernets = (seqState.Current :: seqState.Outside) |> Seq.map (List.rev >> String.Concat)
              Hypernets = seqState.Inside |> Seq.map (List.rev >> String.Concat) })

let parse = Seq.map IPAddress.fromLine

let solvePart1: IPAddress seq -> int = Seq.filter _.supportsTLS >> Seq.length
let solvePart2: IPAddress seq -> int = Seq.filter _.supportsSSL >> Seq.length

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
