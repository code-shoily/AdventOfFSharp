module Year2021Tests

open Solutions.Tests
open Xunit

module TestYear2021Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2021

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(1139, 1103)

        let actual = Year2021.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothInt(1660158, 1604592846)

        let actual = Year2021.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt(1540244, 4203981)

        let actual = Year2021.Day03.solve (getInput day)

        Assert.Equal(expected, actual)
