/// Year 2018/2 - Inventory Management System
/// Link: https://adventofcode.com/2018/day/2
/// Difficulty: s
/// Tags: string
/// Remarks: `StringBuilder` `=` is not transparent.
module Year2018.Day02

open System.Text
open Common.Types

let solvePart1 input =
    let find n =
        Seq.countBy id
        >> Seq.tryFind (snd >> (=) n)
        >> (fun x -> if x.IsNone then 0 else 1)

    let checksum boxID = (boxID |> find 2, boxID |> find 3)

    let a, b =
        ((0, 0), (Seq.map checksum input))
        ||> Seq.fold (fun (a', b') (a, b) -> a' + a, b' + b)

    a * b

let solvePart2 (input: string seq) =
    let getWithRemovedChar (s: string) (at: int) =
        let builder = StringBuilder(s)
        builder.Remove(at, 1).ToString()

    seq { 0 .. (Seq.head input).Length - 1 }
    |> Seq.map (fun i ->
        input
        |> Seq.map (fun s -> getWithRemovedChar s i)
        |> Seq.groupBy id
        |> Seq.map (fun (a, b) -> (a, List.ofSeq b))
        |> Seq.tryFind (fun (_, f) -> Seq.length f > 1))
    |> Seq.pick (id)
    |> fst

let solve (rawInput: string seq) =
    IntString(solvePart1 rawInput, solvePart2 rawInput)
