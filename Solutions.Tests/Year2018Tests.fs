module Year2018Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2018

open Xunit

module TestYear2018Solutions =
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

    let getInput = Helpers.getInput 2018
    let isExpectedFor = Helpers.isExpectedForUtil getSolver 2018

    [<Fact>]
    let ``Solves Day 1`` () = BothInt(590, 83445) |> isExpectedFor 1

    [<Fact>]
    let ``Solves Day 2`` () =
        IntString(7221, "mkcdflathzwsvjxrevymbdpoq") |> isExpectedFor 2

    [<Fact>]
    let ``Solves Day 3`` () = BothInt(110389, 552) |> isExpectedFor 3

    [<Fact>]
    let ``Solves Day 4`` () =
        BothInt(74743, 132484) |> isExpectedFor 4
