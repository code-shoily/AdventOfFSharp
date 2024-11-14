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
