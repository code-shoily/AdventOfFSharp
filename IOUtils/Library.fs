module IOUtils

open System.IO

let getFilePath year day = $"Inputs/%d{year}_%02d{day}.txt"
let fileExists path = File.Exists(path)

/// <summary>
/// Reads an input file given `year` and `day` and returns the content as sequence of lines
/// </summary>
/// <param name="year">Advent of Code year</param>
/// <param name="day">Puzzle day</param>
let readLines year day =
    let filePath = getFilePath year day

    match File.Exists(filePath) with
    | true ->
        seq {
            let filePath = $"Inputs/%d{year}_%02d{day}.txt"
            use sr = new StreamReader(filePath)

            while not sr.EndOfStream do
                yield sr.ReadLine()
        }
        |> Some
    | false -> None
