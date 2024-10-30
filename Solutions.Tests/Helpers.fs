module Solutions.Tests.Helpers

open AdventOfCode.FSharp
open IOUtils

let getInput year day =
    match readLines year day with
    | Some input -> input
    | None -> failwith "Test failed, input file not found"