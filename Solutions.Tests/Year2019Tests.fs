module Year2019Tests

open Solutions.Tests
open Xunit

module TestYear2019Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2019

    [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(3421505, 5129386)

        let actual = Year2019.Day01.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 2`` () =
        let day = 2
        let expected = BothInt(3562624, 8298)

        let actual = Year2019.Day02.solve (getInput day)

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Solves Day 3`` () =
        let day = 3
        let expected = BothInt (1195, 91518)

        let actual = Year2019.Day03.solve (getInput day)

        Assert.Equal(expected, actual)
