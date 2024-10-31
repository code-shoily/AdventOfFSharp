/// Year 2015/2 - I Was Told There Would Be No Math
/// Link: https://adventofcode.com/2015/day/2
/// Difficulty: xs
/// Tags: geometry
/// Remarks:
module Year2015.Day02

open Common.Helpers
open Common.Types

type Dimension =
    { width: int
      height: int
      length: int }

    member this.surfaceArea =
        match this with
        | { length = l; height = h; width = w } -> 2 * (l * h + h * w + w * l)

    member private this.smallestSides =
        [ this.length; this.height; this.width ] |> Seq.sort |> Seq.take 2

    member this.volume = this.length * this.height * this.width
    member this.smallestSideArea = this.smallestSides |> Seq.reduce (*)
    member this.smallestSidePerimeter = this.smallestSides |> Seq.reduce (+) |> ((*) 2)

    static member ofString(s: string) : Dimension =
        match s.Split([| 'x' |]) with
        | [| w; h; l |] ->
            { width = int w
              height = int h
              length = int l }
        | _ -> unreachable ()

let wrappingPaper (d: Dimension) = d.surfaceArea + d.smallestSideArea
let ribbon (d: Dimension) = d.volume + d.smallestSidePerimeter

let parse = Seq.map Dimension.ofString

let solvePart1 = Seq.map wrappingPaper >> Seq.sum
let solvePart2 = Seq.map ribbon >> Seq.sum

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
