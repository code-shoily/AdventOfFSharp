#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/IOUtils/bin/Debug/net9.0/IOUtils.dll"
#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/Common/bin/Debug/net9.0/Common.dll"

open IOUtils

let getInput year day = readLines year day |> Option.get

let rawInput = getInput 2015 5
(* Copy Pasta *)
(* Experiments go here *)
