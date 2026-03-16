module Year2021Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2021

open Xunit

module TestYear2021Solutions =
    let getSolver day =
        match day with
        | 1 -> Day01.solve
        | 2 -> Day02.solve
        | 3 -> Day03.solve
        | _ -> unreachable ()

    let getInput = Helpers.getInput 2021
    let isExpectedFor = Helpers.isExpectedForUtil getSolver 2021

    [<Fact>]
    let ``Solves Day 1`` () = BothInt(1139, 1103) |> isExpectedFor 1

    [<Fact>]
    let ``Solves Day 2`` () =
        BothInt(1660158, 1604592846) |> isExpectedFor 2

    [<Fact>]
    let ``Solves Day 3`` () =
        BothInt(1540244, 4203981) |> isExpectedFor 3
