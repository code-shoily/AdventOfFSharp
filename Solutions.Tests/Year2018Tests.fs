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
