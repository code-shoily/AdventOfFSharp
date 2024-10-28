/// Year 2020/1 - Report Repair
/// Link: https://adventofcode.com/2020/day/1
/// Difficulty: xs
/// Tags: n-sum
module Year2020.Day01

open Common.Helpers
open Common.Types

let twoSum (target: int) (nums: int array) =
    let rec doTwoSum left right =
        if left >= right then
            1
        else
            match nums[left] + nums[right] with
            | result when result = target -> nums[left] * nums[right]
            | larger when larger > target -> doTwoSum left (right - 1)
            | _ -> doTwoSum (left + 1) right

    doTwoSum 0 (nums.Length - 1)

let solvePart1 = twoSum 2020

let solvePart2 =
    let rec doThreeSum (target: int) (subarray: int array) =
        let twoSumResult = twoSum (2020 - target) subarray

        if twoSumResult = 1 then
            doThreeSum subarray[0] subarray[1..]
        else
            target * twoSumResult

    doThreeSum 2020

let parse = Seq.map int >> Seq.sort >> Seq.toArray

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
