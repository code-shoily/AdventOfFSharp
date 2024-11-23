/// Year 2020/4 - Passport Processing
/// Link: https://adventofcode.com/2020/day/4
/// Difficulty: xs
/// Tags: data-modeling validation
/// Remarks:
module Year2020.Day04

open System
open Common.Helpers
open Common.Types

type LengthUnit =
    | CM
    | Inch

type PassportField =
    | Byr of int
    | Iyr of int
    | Eyr of int
    | Hgt of int * LengthUnit
    | Hcl of string
    | Ecl of string
    | Pid of string
    | Cid of int

let toPassportField (line: string) : PassportField =
    let toHeight =
        Seq.toList
        >> List.partition Char.IsDigit
        >> (fun (a, b) -> String.Concat(a) |> int, if String.Concat(b) = "in" then Inch else CM)
        >> Hgt

    match line.Split(":") with
    | [| "byr"; birthYear |] -> Byr(int birthYear)
    | [| "iyr"; issueYear |] -> Iyr(int issueYear)
    | [| "eyr"; expiryYear |] -> Eyr(int expiryYear)
    | [| "hgt"; height |] -> toHeight height
    | [| "hcl"; hairColour |] -> Hcl hairColour
    | [| "ecl"; eyeColour |] -> Ecl eyeColour
    | [| "pid"; passportNumber |] -> Pid passportNumber
    | [| "cid"; countryID |] -> Cid(int countryID)
    | _ -> unreachable ()

let isValidPassport =
    let isValidField (field: PassportField) =
        let isValidHexColour (code: string) =
            code.StartsWith("#")
            && code.Length = 7
            && code
               |> Seq.tail
               |> Seq.forall (fun char ->
                   Char.IsDigit(char) || set [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ] |> Set.contains char)

        let isValidEyeColour eyeColour =
            set [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
            |> Set.contains eyeColour

        let isValidPassportNumber (passportNumber: string) =
            passportNumber.Length = 9 && passportNumber |> Seq.forall Char.IsDigit

        match field with
        | Byr birthYear -> 1920 <= birthYear && birthYear <= 2002
        | Iyr issueYear -> 2010 <= issueYear && issueYear <= 2020
        | Eyr expiryYear -> 2020 <= expiryYear && expiryYear <= 2030
        | Hgt(height, CM) -> 150 <= height && height <= 193
        | Hgt(height, Inch) -> 59 <= height && height <= 76
        | Hcl hairColour -> isValidHexColour hairColour
        | Ecl eyeColour -> isValidEyeColour eyeColour
        | Pid passportNumber -> isValidPassportNumber passportNumber
        | Cid _ -> true

    Seq.forall isValidField

let parse (rawInput: string seq) =
    let hasAllRequiredField (fields: PassportField seq) =
        (7, fields)
        ||> Seq.fold (fun count ->
            function
            | Byr _
            | Iyr _
            | Eyr _
            | Hgt _
            | Hcl _
            | Ecl _
            | Pid _ -> count - 1
            | _ -> count)
        |> (=) 0

    rawInput
    |> Seq.toList
    |> paragraphs
    |> List.map (List.collect (_.Split(" ") >> (Array.map toPassportField) >> Array.toList))
    |> Seq.filter hasAllRequiredField

let solvePart1 = Seq.length
let solvePart2 = Seq.filter isValidPassport >> Seq.length

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
