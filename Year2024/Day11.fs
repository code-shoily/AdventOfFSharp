/// Year 2024/11 - Plutonian Pebbles
/// Link: https://adventofcode.com/2024/day/11
/// Difficulty: s
/// Tags: bignum memoization counter
/// Remarks:
module Year2024.Day11

open System
open Common.Types

let digits: int64 -> int =
    function
    | 0L -> 1
    | n -> 1 + (Math.Log10(float n) |> floor |> int)

let getHalves (n: int64) d =
    let leftHalf = n / (pown 10L d)
    let rightHalf = n % (leftHalf * (pown 10L d))

    (leftHalf, rightHalf)

let (|SinglePebble|PebblePair|) pebbleBefore =
    match (pebbleBefore, digits pebbleBefore) with
    | 0L, _ -> SinglePebble 1L
    | _, even when even % 2 = 0 -> PebblePair(getHalves pebbleBefore (even / 2))
    | n, _ -> SinglePebble(n * 2024L)

module Counter =
    let newPebble engrave howMany counter =
        counter
        |> Map.change engrave (function
            | None -> Some howMany
            | Some n -> Some(n + howMany))

    let removePebble engrave howMany counter =
        match counter |> Map.tryFind engrave with
        | Some(n) when n = howMany -> counter.Remove engrave
        | Some(n) -> counter |> Map.add engrave (n - howMany)
        | None -> counter

    let blink (counter: Map<int64, int64>) =
        (counter, counter)
        ||> Seq.fold (fun counter kv ->
            let pebble, frequency = kv.Key, kv.Value

            match pebble with
            | SinglePebble x -> counter |> removePebble pebble frequency |> newPebble x frequency
            | PebblePair(a, b) ->
                counter
                |> removePebble pebble frequency
                |> newPebble a frequency
                |> newPebble b frequency)

let afterBlinks n input =
    (input, [ 1..n ])
    ||> Seq.fold (fun map _ -> map |> Counter.blink)
    |> Seq.sumBy _.Value

let solvePart1 = afterBlinks 25
let solvePart2 = afterBlinks 75

let parse: (string seq -> Map<int64, int64>) =
    Seq.exactlyOne
    >> _.Split(" ")
    >> Seq.map int64
    >> Seq.countBy id
    >> Seq.map (fun (k, v) -> (k, int64 v))
    >> Map.ofSeq

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothLong(solvePart1 input, solvePart2 input)
