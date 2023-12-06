module AdventOfCode2023.Days.Day01

let numbers =
    [| for i in 0..9 do
           ($"{i}", i) |]

let numbers_english =
    [| "one", 1
       "two", 2
       "three", 3
       "four", 4
       "five", 5
       "six", 6
       "seven", 7
       "eight", 8
       "nine", 9 |]

let calibration_value (possibilities) (line: string) =

    let chooser (key: string, value) =
        match line.IndexOf key with
        | -1 -> None
        | firstIndex ->
            let lastIndex = line.LastIndexOf key

            [| {| index = firstIndex; value = value |}
               {| index = lastIndex; value = value |} |]
            |> Some

    let indices =
        possibilities
        |> Array.choose chooser
        |> Array.concat
        |> Array.sortBy _.index
        |> Array.map _.value

    match Array.tryHead indices, Array.tryLast indices with
    | Some first, Some last -> first * 10 + last
    | _ -> 0

let solve possibilities input =
    input |> Array.map (calibration_value possibilities) |> Array.sum

let public part1 = solve numbers

let part2 = solve (Array.concat [ numbers; numbers_english ])

let example2 =
    """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

let example1 =
    """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"
"""

assert ((example1 |> _.Trim() |> _.Split("\n") |> part1) = 142)
assert ((example2 |> _.Trim() |> _.Split("\n") |> part2) = 281)
