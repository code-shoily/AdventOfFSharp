#r "./IOUtils/bin/Debug/net9.0/IOUtils.dll"
#r "./Common/bin/Debug/net9.0/Common.dll"

open IOUtils

let captureInputAs f year month =
    readLines year month
    |> (Option.map f)

let inputAsString = captureInputAs (String.concat "\n")
let inputAsList = captureInputAs List.ofSeq
