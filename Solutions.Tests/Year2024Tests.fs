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
