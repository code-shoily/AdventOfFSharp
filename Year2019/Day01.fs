/// Year 2019/1 - The Tyranny of the Rocket Equation
/// Link: https://adventofcode.com/2019/day/1
/// Difficulty: xs
/// Tags: formula recursion
/// Remarks:
module Year2019.Day01

open Common.Types

let fuelForMass mass = mass / 3 - 2

let fuelForModule mass =
    let rec compute mass totalFuel =
        match fuelForMass mass with
        | fuel when fuel <= 0 -> totalFuel
        | fuel -> compute fuel (fuel + totalFuel)

    compute mass 0

let fuelCalculator f = f |> Seq.map >> Seq.sum
let solvePart1 = fuelCalculator fuelForMass
let solvePart2 = fuelCalculator fuelForModule

let rec solve (rawInput: string seq) =
    let input = Seq.map (string >> int) rawInput
    BothInt(solvePart1 input, solvePart2 input)
