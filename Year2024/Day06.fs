/// Year 2024/6 - Day 6: Guard Gallivant
/// Link: https://adventofcode.com/2024/day/6
/// Difficulty: m
/// Tags: navigation slow should-refactor
/// Remarks: Add concurrency to part 2
module Year2024.Day06

open Common.Helpers
open Common.Types

type MapItem =
    | Obstacle
    | Path
    | Guard

    static member fromChar =
        function
        | '.' -> Path
        | '#' -> Obstacle
        | '^' -> Guard
        | _ -> unreachable ()

type Direction =
    | Up
    | Down
    | Left
    | Right

type Guard =
    { Position: int * int
      Facing: Direction }

    member this.turn() =
        match this.Facing with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    member this.nextMove() =
        let x, y = this.Position

        match this.Facing with
        | Up -> x - 1, y
        | Down -> x + 1, y
        | Left -> x, y - 1
        | Right -> x, y + 1

    member this.placement() = (this.Facing, this.Position)

let mapStateAt (lab: MapItem[][]) (x, y) =
    if x < 0 || x >= lab.Length || y < 0 || y >= lab[x].Length then
        {| Escape = true; ShouldTurn = false |}
    elif lab[x][y] |> _.IsObstacle then
        {| Escape = false; ShouldTurn = true |}
    else
        {| Escape = false; ShouldTurn = false |}

[<TailCall>]
let rec nextMove lab (guard: Guard) =
    let newPosition = guard.nextMove ()
    let newPositionStatus = mapStateAt lab newPosition

    if newPositionStatus.Escape then
        None
    elif newPositionStatus.ShouldTurn then
        nextMove lab { guard with Facing = guard.turn () }
    else
        Some { guard with Position = newPosition }

[<TailCall>]
let getVisitedPositions lab (guard: Guard) =
    let rec prediction guard visits =
        match (nextMove lab guard) with
        | None -> visits
        | Some({ Position = position } as guard) -> prediction guard (visits |> Set.add position)

    prediction guard (set [ guard.Position ])

[<TailCall>]
let isLooping lab (guard: Guard) =
    let rec prediction guard (visits: Set<Direction * (int * int)>) =
        match (nextMove lab guard) with
        | None -> false
        | Some guard ->
            visits.Contains(guard.placement ())
            || prediction guard (visits |> Set.add (guard.placement ()))

    prediction guard (set [ guard.placement () ])


let solvePart1 = Seq.length >> (+) 1

let solvePart2 (lab: MapItem[][]) (guard: Guard) =
    Seq.filter (fun (x, y) ->
        lab[x][y] <- Obstacle
        let looping = isLooping lab guard
        lab[x][y] <- Path
        looping)
    >> Seq.length

let parse (rawInput: string seq) =
    let labMap =
        rawInput
        |> Array.ofSeq
        |> Array.map (_.ToCharArray() >> Array.map MapItem.fromChar)

    let size = labMap |> Array.length
    let getCoords (idx: int) = (idx / size), (idx % size)
    let position = labMap |> Array.collect id |> Array.findIndex _.IsGuard |> getCoords

    labMap, { Position = position; Facing = Up }

let solve (rawInput: string seq) =
    let lab, guard = parse rawInput

    let positionsVisited =
        getVisitedPositions lab guard |> Set.remove guard.Position |> Set.toSeq

    BothInt(solvePart1 positionsVisited, solvePart2 lab guard positionsVisited)
