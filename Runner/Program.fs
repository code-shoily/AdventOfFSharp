open IOUtils
open Common.Types

let isValid year day =
    year >= 2015 && year <= 2024 && day >= 1 && day <= 25

let solutionFor year day =
    match readLines year day with
    | Some input ->
        match year with
        | 2015 -> Year2015.Solver.solveFor input day
        | 2016 -> Year2016.Solver.solveFor input day
        | 2017 -> Year2017.Solver.solveFor input day
        | 2018 -> Year2018.Solver.solveFor input day
        | 2019 -> Year2019.Solver.solveFor input day
        | 2020 -> Year2020.Solver.solveFor input day
        | 2021 -> Year2021.Solver.solveFor input day
        | 2022 -> Year2022.Solver.solveFor input day
        | 2023 -> Year2023.Solver.solveFor input day
        | 2024 -> Year2024.Solver.solveFor input day
        | _ -> Error(NotDoneYet)
    | None -> Error(FileNotFound)


[<EntryPoint>]
let main argv =
    let year, day =
        if argv.Length = 2 then
            (int argv[0], int argv[1])
        else
            (2015, 6)

    let result =
        if isValid year day then
            solutionFor year day
        else
            Error(InvalidInput)

    printfn $"%A{result}"

    0
