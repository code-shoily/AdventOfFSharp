/// Year 2023/1 - Trebuchet?!
/// Link: https://adventofcode.com/2023/day/1
/// Difficulty: xs
/// Tags: string
/// Remarks:
module Year2023.Day01

open Common.Types

let calibrationValue =
    List.choose id
    >> (function
    | [ a ] -> 10 * a + a
    | first :: rest -> 10 * first + List.last rest
    | [] -> 0)

let collectNumbers getLeadingNumber word =
    let rec collect (word: string) result =
        if word = "" then
            List.rev result
        else
            collect word[1..] (getLeadingNumber word :: result)

    collect word [] |> calibrationValue

let solvePart1 =
    let getLeadingNumber: string -> int option =
        function
        | s when s.StartsWith("0") -> Some 0
        | s when s.StartsWith("1") -> Some 1
        | s when s.StartsWith("2") -> Some 2
        | s when s.StartsWith("3") -> Some 3
        | s when s.StartsWith("4") -> Some 4
        | s when s.StartsWith("5") -> Some 5
        | s when s.StartsWith("6") -> Some 6
        | s when s.StartsWith("7") -> Some 7
        | s when s.StartsWith("8") -> Some 8
        | s when s.StartsWith("9") -> Some 9
        | _ -> None

    Seq.map (collectNumbers getLeadingNumber) >> Seq.sum

let solvePart2 =
    let getLeadingNumber: string -> int option =
        function
        | s when s.StartsWith("0") || s.StartsWith("zero") -> Some 0
        | s when s.StartsWith("1") || s.StartsWith("one") -> Some 1
        | s when s.StartsWith("2") || s.StartsWith("two") -> Some 2
        | s when s.StartsWith("3") || s.StartsWith("three") -> Some 3
        | s when s.StartsWith("4") || s.StartsWith("four") -> Some 4
        | s when s.StartsWith("5") || s.StartsWith("five") -> Some 5
        | s when s.StartsWith("6") || s.StartsWith("six") -> Some 6
        | s when s.StartsWith("7") || s.StartsWith("seven") -> Some 7
        | s when s.StartsWith("8") || s.StartsWith("eight") -> Some 8
        | s when s.StartsWith("9") || s.StartsWith("nine") -> Some 9
        | _ -> None

    Seq.map (collectNumbers getLeadingNumber) >> Seq.sum


let solve (rawInput: string seq) =
    let input = rawInput |> List.ofSeq
    BothInt(solvePart1 input, solvePart2 input)
