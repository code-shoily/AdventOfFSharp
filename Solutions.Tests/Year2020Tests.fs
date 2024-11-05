module Year2020Tests

open Solutions.Tests
open Xunit

module TestYear2020Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2020

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(1014624, 80072256)

        let actual = Year2020.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothInt(607, 321)

        let actual = Year2020.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothLong(272L, 3898725600L)

        let actual = Year2020.Day03.solve (getInput day)

        Assert.Equal(expected, actual)
