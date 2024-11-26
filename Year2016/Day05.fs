/// Year 2016/5 - How About a Nice Game of Chess?
/// Link: https://adventofcode.com/2016/day/5
/// Difficulty: s
/// Tags: md5 slow should-refactor
/// Remarks: Why is it 5x slower when running main?
module Year2016.Day05

open System
open System.Security.Cryptography
open System.Text
open Common.Types

let findHashFor secret token =
    let md5 = MD5.Create()

    md5.ComputeHash(token |> string |> (+) secret |> Encoding.ASCII.GetBytes)
    |> BitConverter.ToString
    |> _.Replace("-", "")

let solvePart1 (secret: string) =
    Seq.initInfinite id
    |> Seq.map (findHashFor secret)
    |> Seq.filter _.StartsWith("00000")
    |> Seq.map (fun s -> s[5])
    |> Seq.take (8)
    |> String.Concat

let solvePart2 (secret: string) =
    let toHexInt =
        function
        | ch when Char.IsDigit(ch) -> int (string ch) |> Some
        | 'a' -> Some 10
        | 'b' -> Some 11
        | 'c' -> Some 12
        | 'd' -> Some 13
        | 'e' -> Some 14
        | 'f' -> Some 15
        | _ -> None

    let rec getFiveZeroPrefixFrom (number: int) =
        let hash = number |> findHashFor secret

        if hash.StartsWith("00000") then
            number, (toHexInt hash[5]), hash[6]
        else
            getFiveZeroPrefixFrom (number + 1)

    let rec decryptPassword number positions result =
        let nextNumber, pos, value as tuple = getFiveZeroPrefixFrom number

        if Set.isEmpty positions then
            result
        elif positions |> Set.contains pos then
            decryptPassword (nextNumber + 1) (positions |> Set.remove pos) ((pos, value) :: result)
        else
            decryptPassword (nextNumber + 1) positions result

    let positions =
        set [ Some 0; Some 1; Some 2; Some 3; Some 4; Some 5; Some 6; Some 7 ]

    decryptPassword 1 positions []
    |> List.sortBy (fst >> _.Value)
    |> List.map snd
    |> String.Concat

let parse = Seq.head

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothString(solvePart1 input, solvePart2 input)
