module Year2023Tests

open Solutions.Tests
open Xunit

module TestYear2023Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2023

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(53194, 54249)

        let actual = Year2023.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothInt(2085, 79315)

        let actual = Year2023.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt(539713, 84159075)

        let actual = Year2023.Day03.solve (getInput day)

        Assert.Equal(expected, actual)
