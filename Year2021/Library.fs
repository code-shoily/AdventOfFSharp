﻿namespace Year2021

open Common.Types

module Solver =
    let solveFor input day =
        match day with
        | 1 -> Day01.solve input |> Ok
        | _ -> NotDoneYet |> Error
