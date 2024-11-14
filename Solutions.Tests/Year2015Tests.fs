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

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothInt(1606483, 3842356)

        let actual = Year2015.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt(2081, 2341)

        let actual = Year2015.Day03.solve (getInput day)

        Assert.Equal(expected, actual)
