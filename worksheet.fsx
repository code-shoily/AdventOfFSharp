﻿#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/IOUtils/bin/Debug/net9.0/IOUtils.dll"
#r "C:/Users/mafin/repos/fsharp/AdventOfFSharp/Common/bin/Debug/net9.0/Common.dll"

open System.Collections
open System.Text
open System.Text.RegularExpressions
open IOUtils

let getInput year day = readLines year day |> Option.get

open System
open System.Security.Cryptography
open Common.Types
open Common.Helpers

let rawInput = getInput 2024 1

(* Copy Pasta *)

(* Experiments go here *)
solve rawInput
