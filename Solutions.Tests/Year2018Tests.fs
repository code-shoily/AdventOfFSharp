module Year2018Tests

open Solutions.Tests
open Xunit

module TestYear2018Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2018

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(590, 83445)

        let actual = Year2018.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = IntString(7221, "mkcdflathzwsvjxrevymbdpoq")

        let actual = Year2018.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt(110389, 552)

        let actual = Year2018.Day03.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 4`` () =
        let day = 4
        let expected = BothInt(74743, 132484)

        let actual = Year2018.Day04.solve (getInput day)

        Assert.Equal(expected, actual)
