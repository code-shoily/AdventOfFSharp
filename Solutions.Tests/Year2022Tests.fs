module Year2022Tests

open Solutions.Tests
open Xunit

module TestYear2022Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2022

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(70720, 207148)

        let actual = Year2022.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothInt(12645, 11756)

        let actual = Year2022.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt(8233, 2821)

        let actual = Year2022.Day03.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 4`` () =
        let day = 4
        let expected = BothInt(518, 909)

        let actual = Year2022.Day04.solve (getInput day)

        Assert.Equal(expected, actual)
