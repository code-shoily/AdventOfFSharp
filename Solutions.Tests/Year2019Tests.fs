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
