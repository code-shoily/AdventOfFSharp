#r "./IOUtils/bin/Debug/net10.0/IOUtils.dll"
#r "./Common/bin/Debug/net10.0/Common.dll"
#r "./Meta/bin/Debug/net10.0/Meta.dll"

open System.IO
open IOUtils

let captureInputAs f year month = readLines year month |> (Option.map f)

let inputAsString = captureInputAs (String.concat "\n")
let inputAsList = captureInputAs List.ofSeq

#r "nuget: Yog.FSharp, 0.5.0"
#r "./Year2018/bin/Debug/net10.0/Year2018.dll"

open Year2018.Day07
let input = captureInputAs id 2018 7

match input with
| Some lines -> 
    let result = solve lines
    printfn $"%A{result}"
| None -> failwith "No input found"
