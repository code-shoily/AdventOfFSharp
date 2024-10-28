module Year2015Tests

open Solutions.Tests
open Xunit

module TestYear2015Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2015

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(232, 1783)

        let actual = Year2015.Day01.solve (getInput day)

        Assert.Equal(expected, actual)
