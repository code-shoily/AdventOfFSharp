module Year2020Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2020

open Xunit

module TestYear2020Solutions =
    let getSolver day =
        match day with
        | 1 -> Day01.solve
        | 2 -> Day02.solve
        | 3 -> Day03.solve
        | 4 -> Day04.solve
        | 5 -> Day05.solve
        | 6 -> Day06.solve
        | 7 -> Day07.solve
        | _ -> unreachable ()

    let getInput = Helpers.getInput 2020
    let isExpectedFor = Helpers.isExpectedForUtil getSolver 2020

    [<Fact>]
    let ``Solves Day 1`` () =
        BothInt(1014624, 80072256) |> isExpectedFor 1

    [<Fact>]
    let ``Solves Day 2`` () = BothInt(607, 321) |> isExpectedFor 2

    [<Fact>]
    let ``Solves Day 3`` () =
        BothLong(272L, 3898725600L) |> isExpectedFor 3

    [<Fact>]
    let ``Solves Day 4`` () = BothInt(233, 111) |> isExpectedFor 4

    [<Fact>]
    let ``Solves Day 5`` () = BothInt(930, 515) |> isExpectedFor 5

    [<Fact>]
    let ``Solves Day 6`` () = BothInt(6885, 3550) |> isExpectedFor 6

    [<Fact>]
    let ``Solves Day 7`` () = BothInt(355, 5312) |> isExpectedFor 7
