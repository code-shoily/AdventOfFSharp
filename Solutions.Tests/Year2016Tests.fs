module Year2016Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2016

open Xunit

module TestYear2016Solutions =
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

    let getInput = Helpers.getInput 2016
    let isExpectedFor = Helpers.isExpectedForUtil getSolver 2016

    [<Fact>]
    let ``Solves Day 1`` () = BothInt(253, 126) |> isExpectedFor 1

    [<Fact>]
    let ``Solves Day 2`` () =
        BothString("76792", "A7AC3") |> isExpectedFor 2

    [<Fact>]
    let ``Solves Day 3`` () = BothInt(993, 1849) |> isExpectedFor 3

    [<Fact>]
    let ``Solves Day 4`` () = BothInt(158835, 993) |> isExpectedFor 4

    [<Fact(Skip = "Slow")>]
    let ``Solves Day 5`` () =
        BothString("F77A0E6E", "999828EC") |> isExpectedFor 5

    [<Fact>]
    let ``Solves Day 6`` () =
        BothString("qzedlxso", "ucmifjae") |> isExpectedFor 6


    [<Fact>]
    let ``Solves Day 7`` () = BothInt(105, 258) |> isExpectedFor 7
