module Solutions.Tests.Helpers

open IOUtils

open FsUnit.Xunit

let getInput year day =
    match readLines year day with
    | Some input -> input
    | None -> failwith "Test failed, input file not found"

let isExpectedForUtil solver year day result =
    day |> getInput year |> solver day |> should equal result
