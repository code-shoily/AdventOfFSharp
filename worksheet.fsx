#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/IOUtils/bin/Debug/net9.0/IOUtils.dll"
#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/Common/bin/Debug/net9.0/Common.dll"
// #r "nuget: FSharpx.Collections"

open System
open System.Collections
open System.Globalization
open System.Security.AccessControl
open System.Text
open System.Text.RegularExpressions
open Common.Types
open Common.Helpers
open IOUtils

let getInput year day = readLines year day |> Option.get

let rawInput = getInput 2024 25
(* Copy Pasta *)
