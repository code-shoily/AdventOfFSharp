namespace Meta

open System.IO
open System

module SrcHelper =
    let parseTitle (title: string) =
        let parseLeft (yearInfo: string) =
            match yearInfo.Replace("Year", "").TrimStart().Split("/") with
            | [| year; day |] -> Some((int year), (int day))
            | _ -> None

        if title.TrimEnd().EndsWith("???") then
            None
        else
            match title.Split("-", StringSplitOptions.TrimEntries) with
            | [| yearInfo; title |] ->
                let year, day = (parseLeft yearInfo) |> Option.get
                Some(year, day, title)

            | _ -> None

    let extractComments year day baseDir =
        Path.Join(baseDir, $"Year{year}", $"Day%02d{day}.fs")
        |> File.ReadLines
        |> Seq.take 5
        |> List.ofSeq

module Solution =
    type Difficulty =
        | XS
        | S
        | M
        | L
        | XL
        | NA

        static member FromString =
            function
            | "xs" -> XS
            | "s" -> S
            | "m" -> M
            | "l" -> L
            | "xl" -> XL
            | _ -> NA

    type Stats =
        { Year: int
          Day: int
          Title: string
          Difficulty: Difficulty
          Tags: string[]
          Remarks: string }

        static member FromSrcFile year day basePath =
            let tokens =
                basePath
                |> SrcHelper.extractComments year day
                |> List.map (
                    _.Replace("///", "").TrimStart().Split(":", StringSplitOptions.TrimEntries)
                    >> Array.last
                )

            match SrcHelper.parseTitle tokens[0] with
            | Some(year, day, title) ->
                match tokens with
                | [ _; _; difficulty; tags; remarks ] ->
                    { Year = year
                      Day = day
                      Title = title
                      Difficulty = Difficulty.FromString(difficulty.ToLower())
                      Tags = tags.Split(" ")
                      Remarks = remarks }
                    |> Some
                | _ -> failwith "Invalid parsing"
            | _ -> None
