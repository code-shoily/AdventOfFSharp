/// Year 2018/4 - Repose Record
/// Link: https://adventofcode.com/2018/day/4
/// Difficulty: s
/// Tags: frequency date-time regex
/// Remarks:
module Year2018.Day04

open System
open System.Text.RegularExpressions
open Common.Types

[<AutoOpen>]
module Types =
    let re =
        @"\[(?<date>\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (?<status>falls asleep|wakes up|Guard #(?<guard>\d+) begins shift)"
        |> Regex

    type Status =
        | WokeUp
        | FellAsleep
        | BeganShift

        static member fromString =
            function
            | "wakes up" -> WokeUp
            | "falls asleep" -> FellAsleep
            | _ -> BeganShift

    type LogInfo =
        { Date: DateTime
          Status: Status
          GuardID: int option }

        static member fromLine(log: string) =
            let captures = log |> re.Match |> _.Groups

            let guardID =
                match captures["guard"].Value with
                | "" -> None
                | idString -> Some <| int idString

            { Date = DateTime.Parse(captures["date"].Value)
              Status = Status.fromString (captures["status"].Value)
              GuardID = guardID }

    type SleepRecord =
        { Guard: int
          Duration: int
          Tally: (int * int) list }

        member this.mode =
            let minute, frequency = List.maxBy snd <| this.Tally

            {| Minute = minute
               Frequency = frequency |}

        member this.result = this.Guard * this.mode.Minute

        member this.hasSlept = this.Duration > 0

    type Solver = SleepRecord list -> int

[<AutoOpen>]
module Builders =
    let updateGuardIDs (logs: LogInfo seq) =
        let firstLog = Seq.head logs

        ((firstLog.GuardID, []), logs)
        ||> Seq.fold (fun (lastGuard, newLogs) log ->
            match log.GuardID with
            | None -> (lastGuard, { log with GuardID = lastGuard } :: newLogs)
            | guardID -> (guardID, log :: newLogs))
        |> snd
        |> List.rev

    let collectSleepTimes (guardLogs: LogInfo list) =
        let sleepDuration (startTime: DateTime, endTime: DateTime) =
            let fromMinute, duration =
                startTime.Minute, int <| endTime.Subtract(startTime).TotalMinutes

            List.map (fun i -> (fromMinute + i) % 60) [ 0..duration ]

        let sleepMinutes =
            ((List.empty, None), guardLogs)
            ||> List.fold (fun (sleepLogs, sleptAt as acc) log ->
                match log.Status with
                | FellAsleep -> (sleepLogs, Some log.Date)
                | WokeUp -> ((sleptAt.Value, log.Date) :: sleepLogs), None
                | _ -> acc)
            |> fst
            |> List.collect (sleepDuration >> List.countBy id)

        { Guard = guardLogs.Head.GuardID.Value
          Duration = sleepMinutes |> List.sumBy snd
          Tally = sleepMinutes |> List.countBy fst }

let solvePart1: Solver = List.maxBy _.Duration >> _.result

let solvePart2: Solver =
    List.filter _.hasSlept >> List.maxBy _.mode.Frequency >> _.result

let parse: string seq -> SleepRecord list =
    Seq.map LogInfo.fromLine
    >> Seq.sortBy _.Date
    >> updateGuardIDs
    >> List.groupBy _.GuardID
    >> List.map (snd >> updateGuardIDs >> collectSleepTimes)

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
