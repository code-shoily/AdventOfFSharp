/// Year 2015/4 - The Ideal Stocking Stuffer
/// Link: https://adventofcode.com/2015/day/4
/// Difficulty: xs
/// Tags: md5
/// Remarks:
module Year2015.Day04

open System
open System.Security.Cryptography
open System.Text

open Common.Types

let solve (rawInput: string seq) =
    let secret = rawInput |> Seq.head
    let md5 = MD5.Create()

    let getMD5 (token: int) =
        md5.ComputeHash(token |> string |> (+) secret |> Encoding.ASCII.GetBytes)
        |> BitConverter.ToString
        |> _.Replace("-", "")

    let findHashFor (prefix: string) =
        Seq.initInfinite id |> Seq.map getMD5 |> Seq.findIndex _.StartsWith(prefix)

    BothInt(findHashFor "00000", findHashFor "000000")
