module Year2017Tests

open Solutions.Tests
open Xunit

module TestYear2017Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2017

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(1089, 1156)

        let actual = Year2017.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothInt(32020, 236)

        let actual = Year2017.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt(430, 312453)

        let actual = Year2017.Day03.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 4`` () =
        let day = 4
        let expected = BothInt(455, 186)

        let actual = Year2017.Day04.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 5`` () =
        let day = 5
        let expected = BothInt(372671, 25608480)

        let actual = Year2017.Day05.solve (getInput day)

        Assert.Equal(expected, actual)
