module Year2019Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2019

open Xunit

module TestYear2019Solutions =
    let getSolver day =
        match day with
        | 1 -> Day01.solve
        | 2 -> Day02.solve
        | 3 -> Day03.solve
        | _ -> unreachable ()

    let getInput = Helpers.getInput 2019
    let isExpectedFor = Helpers.isExpectedForUtil getSolver 2019

    [<Fact>]
    let ``Solves Day 1`` () =
        BothInt(3421505, 5129386) |> isExpectedFor 1

    [<Fact>]
    let ``Solves Day 2`` () =
        BothInt(3562624, 8298) |> isExpectedFor 2

    [<Fact>]
    let ``Solves Day 3`` () = BothInt(1195, 91518) |> isExpectedFor 3
