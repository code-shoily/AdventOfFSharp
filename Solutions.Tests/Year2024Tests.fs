module Year2024Tests

open Common.Types
open Solutions.Tests
open Xunit

module TestYear2024Solutions =

    let getInput = Helpers.getInput 2024

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(2742123, 21328497)

        let actual = Year2024.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothInt(486, 540)

        let actual = Year2024.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt(190604937, 82857512)

        let actual = Year2024.Day03.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 4`` () =
        let day = 4
        let expected = BothInt(2575, 2041)

        let actual = Year2024.Day04.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 5`` () =
        let day = 5
        let expected = BothInt(5391, 6142)

        let actual = Year2024.Day05.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact(Skip = "Slow")>]
    let ``Solves Day 6`` () =
        let day = 6
        let expected = BothInt(4982, 1663)

        let actual = Year2024.Day06.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 7`` () =
        let day = 7
        let expected = BothLong(882304362421L, 145149066755184L)

        let actual = Year2024.Day07.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 8`` () =
        let day = 8
        let expected = BothInt(291, 1015)

        let actual = Year2024.Day08.solve (getInput day)

        Assert.Equal(expected, actual)
