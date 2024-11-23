#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/IOUtils/bin/Debug/net9.0/IOUtils.dll"
#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/Common/bin/Debug/net9.0/Common.dll"

open IOUtils

let getInput year day = readLines year day |> Option.get

open System
open Common.Types
open Common.Helpers

let rawInput = getInput 2020 4
(* Copy Pasta *)
(* Experiments go here *)
