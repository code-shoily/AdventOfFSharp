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
