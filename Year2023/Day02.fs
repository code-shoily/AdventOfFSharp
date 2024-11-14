/// Year 2023/2 - Cube Conundrum
/// Link: https://adventofcode.com/2023/day/2
/// Difficulty: s
/// Tags: reduction
/// Remarks:
module Year2023.Day02

open Common.Helpers
open Common.Types
open System.Text.RegularExpressions

type Cube =
    | Red of int
    | Green of int
    | Blue of int

let parse =
    let totalCubesShown (rep: string) =
        let getCubeCount (rep: string) =
            match rep.Split(" ") with
            | [| amt; "red" |] -> Red(int amt)
            | [| amt; "green" |] -> Green(int amt)
            | [| amt; "blue" |] -> Blue(int amt)
            | _ -> unreachable ()

        rep.Split("; ") |> Seq.collect (_.Split(", ") >> Seq.map getCubeCount)

    let cubesPerShow (line: string) =
        let regex = Regex(@"Game (\d+): (.+)")
        let matchLine = regex.Match(line)
        int matchLine.Groups[1].Value, totalCubesShown matchLine.Groups[2].Value

    Seq.map cubesPerShow

let reduceToMostCubes (game, bag) =
    let maxVals =
        ((0, 0, 0), bag)
        ||> Seq.fold (fun (red, green, blue) ->
            function
            | Red n -> (max red n, green, blue)
            | Green n -> (red, max green n, blue)
            | Blue n -> (red, green, max blue n))

    (game, maxVals)

let solvePart1 input =
    input
    |> Seq.map reduceToMostCubes
    |> Seq.filter (fun (_, (r, g, b)) -> r <= 12 && g <= 13 && b <= 14)
    |> Seq.map fst
    |> Seq.sum

let solvePart2 input =
    input
    |> Seq.map reduceToMostCubes
    |> Seq.map (fun (_, (r, g, b)) -> r * g * b)
    |> Seq.sum

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
