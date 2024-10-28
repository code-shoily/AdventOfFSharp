module Solutions.Tests.Helpers

open AdventOfCode.FSharp
open Utils

let getInput year day =
    match readLines year day with
    | Some input -> input
    | None -> failwith "Test failed, input file not found"