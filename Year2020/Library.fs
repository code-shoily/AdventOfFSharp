namespace Year2020

open Common.Types

module Solver =
    let solveFor input day =
        match day with
        | 1 -> input |> Day01.solve |> Ok
        | 2 -> input |> Day02.solve |> Ok
        | 3 -> input |> Day03.solve |> Ok
        | 4 -> input |> Day04.solve |> Ok
        | 5 -> input |> Day05.solve |> Ok
        | 6 -> input |> Day06.solve |> Ok
        | _ -> NotDoneYet |> Error
