module Year2022Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2022

open Xunit

module TestYear2022Solutions =
    let getSolver day =
        match day with
        | 1 -> Day01.solve
        | 2 -> Day02.solve
        | 3 -> Day03.solve
        | 4 -> Day04.solve
        | _ -> unreachable ()

    let getInput = Helpers.getInput 2022
    let isExpectedFor = Helpers.isExpectedForUtil getSolver 2022

    [<Fact>]
    let ``Solves Day 1`` () =
        BothInt(70720, 207148) |> isExpectedFor 1

    [<Fact>]
    let ``Solves Day 2`` () =
        BothInt(12645, 11756) |> isExpectedFor 2

    [<Fact>]
    let ``Solves Day 3`` () = BothInt(8233, 2821) |> isExpectedFor 3

    [<Fact>]
    let ``Solves Day 4`` () = BothInt(518, 909) |> isExpectedFor 4
