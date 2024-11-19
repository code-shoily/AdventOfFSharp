/// Year 2015/5 - Doesn't He Have Intern-Elves For This?
/// Link: https://adventofcode.com/2015/day/5
/// Difficulty: xs
/// Tags: string
/// Remarks:
module Year2015.Day05

open Common.Types

let solvePart1 =
    let containsThreeVowels (word: string) =
        (0, word)
        ||> Seq.scan (fun count ->
            function
            | 'a'
            | 'e'
            | 'i'
            | 'o'
            | 'u' -> count + 1
            | _ -> count)
        |> Seq.tryFind ((<=) 3)
        |> _.IsSome

    let twiceInARow (word: string) =
        word |> Seq.pairwise |> Seq.tryFind (fun (x, y) -> x = y) |> _.IsSome

    let doesNotContainNaughtyPairs (word: string) =
        let naughtyPairs = set [ "ab"; "cd"; "pq"; "xy" ]

        word
        |> Seq.pairwise
        |> Seq.tryFind (fun (x, y) -> naughtyPairs |> Set.contains ((string x, string y) ||> (+)))
        |> _.IsSome
        |> not

    let isNice (word: string) =
        (containsThreeVowels word)
        && (twiceInARow word)
        && (doesNotContainNaughtyPairs word)

    Seq.filter isNice >> Seq.length

let solvePart2 =
    let appearsTwice (word: string) =
        let pairs = Seq.pairwise word
        let placeholder = ('_', '_')

        ({| Visits = set []
            Found = false
            Identical = ('_', '_') |},
         pairs)
        ||> Seq.scan (fun state ((x, y) as pair) ->
            let visits, identical = state.Visits, state.Identical

            if x = y then
                if pair <> identical then
                    {| state with
                        Found = visits |> Set.contains pair
                        Visits = visits |> Set.add pair
                        Identical = pair |}
                else
                    {| state with Identical = placeholder |}
            else
                {| state with
                    Found = visits |> Set.contains pair
                    Visits = visits |> Set.add pair
                    Identical = placeholder |}

        )
        |> Seq.tryFind _.Found
        |> _.IsSome

    let isBetween (word: string) =
        word
        |> Seq.windowed 3
        |> Seq.countBy (function
            | [| left; _; right |] -> left = right
            | _ -> false)
        |> Seq.tryFind (fun (v, c) -> v && c >= 1)
        |> Option.isSome

    let isNice (word: string) = (appearsTwice word) && (isBetween word)

    Seq.filter isNice >> Seq.length


let solve (rawInput: string seq) =
    BothInt(solvePart1 rawInput, solvePart2 rawInput)
