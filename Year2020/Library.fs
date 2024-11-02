namespace Year2020

open Common.Types

module Solver =
    let solveFor input day =
        match day with
        | 1 -> input |> Day01.solve |> Ok
        | 2 -> input |> Day02.solve |> Ok
        | _ -> NotDoneYet |> Error
