module Year2023Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2023

open Xunit

module TestYear2023Solutions =
    let getSolver day =
        match day with
        | 1 -> Day01.solve
        | 2 -> Day02.solve
        | 3 -> Day03.solve
        | _ -> unreachable ()

    let getInput = Helpers.getInput 2023
    let isExpectedFor = Helpers.isExpectedForUtil getSolver 2023

    [<Fact>]
    let ``Solves Day 1`` () =
        BothInt(53194, 54249) |> isExpectedFor 1

    [<Fact>]
    let ``Solves Day 2`` () = BothInt(2085, 79315) |> isExpectedFor 2

    [<Fact>]
    let ``Solves Day 3`` () =
        BothInt(539713, 84159075) |> isExpectedFor 3
