/// Year 2016/4 - Security Through Obscurity
/// Link: https://adventofcode.com/2016/day/4
/// Difficulty: xs
/// Tags: modular-algebra ascii
/// Remarks:
module Year2016.Day04

open System
open Common.Helpers
open Common.Types

module Helpers =
    let charFreq =
        Seq.groupBy id
        >> Seq.map (fun (k, v) -> (k, Seq.length v))
        >> Seq.sortWith (fun (a, b) ->
            function
            | x, y when b = y -> if a < x then -1 else 1
            | _, y when b < y -> 1
            | _ -> -1)

    let computeChecksum (encryptedRoom: string) =
        encryptedRoom |> charFreq |> Seq.take 5 |> Seq.map fst |> String.Concat

type Room =
    { EncryptedName: string
      ID: int
      Checksum: string }

    member this.isValid = this.Checksum = (this.EncryptedName |> Helpers.computeChecksum)

    member this.decrypt =
        let decryptChar ch =
            let charIndex = (int ch) % (int 'a')
            let outputIndex = charIndex + this.ID

            char ((int 'a') + (outputIndex % 26))

        this.EncryptedName |> Seq.map decryptChar |> String.Concat

    static member fromLine(line: string) =
        let tokens = line.Split("-")
        let encryptedName = tokens[0 .. (tokens.Length - 2)] |> String.Concat

        let id, checksum =
            match (Array.last tokens).Replace("]", "").Split("[") with
            | [| idString; checksum |] -> (int idString, checksum)
            | _ -> unreachable ()

        { EncryptedName = encryptedName
          ID = id
          Checksum = checksum }


let parse: string seq -> Room seq = Seq.map Room.fromLine

let solvePart1: Room seq -> int = Seq.filter _.isValid >> Seq.map _.ID >> Seq.sum

let solvePart2: Room seq -> int =
    Seq.find _.decrypt.StartsWith("northpoleobjects") >> _.ID

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
