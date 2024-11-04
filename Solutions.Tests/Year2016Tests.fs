module Year2016Tests

open Solutions.Tests
open Xunit

module TestYear2016Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2016

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(253, 126)

        let actual = Year2016.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothString("76792", "A7AC3")

        let actual = Year2016.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt(993, 1849)

        let actual = Year2016.Day03.solve (getInput day)

        Assert.Equal(expected, actual)
