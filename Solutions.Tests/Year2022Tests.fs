module Year2022Tests

open Solutions.Tests
open Xunit

module TestYear2022Solutions =
    open Common.Types

    let getInput = Helpers.getInput 2022

    (* [<Fact>]
    let ``Solves Day 1`` () =
        let day = 1
        let expected = BothInt(232, 1783)

        let actual = Year2015.Day01.solve (getInput day)

        Assert.Equal(expected, actual) *)