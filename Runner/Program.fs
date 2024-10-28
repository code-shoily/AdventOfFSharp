open AdventOfCode.FSharp
open Utils
open Common.Types

let isValid year day =
    year >= 2015 && year <= 2023 && day >= 1 && day <= 25

let solutionFor year day =
    match readLines year day with
    | Some input ->
        match year with
        | 2015 -> Year2015.Solver.solveFor input day
        | 2016 -> Error(NotDoneYet)
        | 2017 -> Error(NotDoneYet)
        | 2018 -> Error(NotDoneYet)
        | 2019 -> Error(NotDoneYet)
        | 2020 -> Error(NotDoneYet)
        | 2021 -> Error(NotDoneYet)
        | 2022 -> Error(NotDoneYet)
        | 2023 -> Error(NotDoneYet)
        | _ -> Error(NotDoneYet)
    | None -> Error(FileNotFound)


[<EntryPoint>]
let main argv =
    let year = int argv[0]
    let day = int argv[1]

    let result =
        if isValid year day then
            solutionFor year day
        else
            Error(InvalidInput)

    printfn $"%A{result}"
    0
