module Year2024Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2024

open Xunit

module TestYear2024Solutions =
    let getSolver day =
        match day with
        | 1 -> Day01.solve
        | 2 -> Day02.solve
        | 3 -> Day03.solve
        | 4 -> Day04.solve
        | 5 -> Day05.solve
        | 6 -> Day06.solve
        | 7 -> Day07.solve
        | 8 -> Day08.solve
        | 9 -> Day09.solve
        | 10 -> Day10.solve
        | 11 -> Day11.solve
        | 12 -> Day12.solve
        | 13 -> Day13.solve
        | 14 -> Day14.solve
        | 15 -> Day15.solve
        | 16 -> Day16.solve
        | 17 -> Day17.solve
        | 18 -> Day18.solve
        | 19 -> Day19.solve
        | 20 -> Day20.solve
        | 21 -> Day21.solve
        | 22 -> Day22.solve
        | 23 -> Day23.solve
        | 24 -> Day24.solve
        | 25 -> Day25.solve
        | _ -> unreachable ()

    let getInput = Helpers.getInput 2024
    let isExpectedFor = Helpers.isExpectedForUtil getSolver 2024

    [<Fact>]
    let ``Solves Day 1`` () =
        BothInt(2742123, 21328497) |> isExpectedFor 1

    [<Fact>]
    let ``Solves Day 2`` () = BothInt(486, 540) |> isExpectedFor 2

    [<Fact>]
    let ``Solves Day 3`` () =
        BothInt(190604937, 82857512) |> isExpectedFor 3

    [<Fact>]
    let ``Solves Day 4`` () = BothInt(2575, 2041) |> isExpectedFor 4

    [<Fact>]
    let ``Solves Day 5`` () = BothInt(5391, 6142) |> isExpectedFor 5

    [<Fact(Skip = "Slow")>]
    let ``Solves Day 6`` () = BothInt(4982, 1663) |> isExpectedFor 6

    [<Fact>]
    let ``Solves Day 7`` () =
        BothLong(882304362421L, 145149066755184L) |> isExpectedFor 7

    [<Fact>]
    let ``Solves Day 8`` () = BothInt(291, 1015) |> isExpectedFor 8

    [<Fact>]
    let ``Solves Day 10`` () = BothInt(617, 1477) |> isExpectedFor 10

    [<Fact>]
    let ``Solves Day 11`` () =
        BothLong(172484L, 205913561055242L) |> isExpectedFor 11

    [<Fact>]
    let ``Solves Day 25`` () =
        IntString(3146, "🎉") |> isExpectedFor 25
