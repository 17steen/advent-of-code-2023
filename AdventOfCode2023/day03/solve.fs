module AdventOfCode2023.Days.Day03
open AdventOfCode2023

open System

type Tile =
    | Digit of char
    | Empty
    | Symbol of char





let parse_tile ch : Tile =
    match ch with
    | digit when System.Char.IsAsciiDigit(digit) -> Digit digit
    | '.' -> Empty
    | ch -> Symbol ch

let sibling_indices i j =
    seq {
        (i - 1, j - 1)
        (i - 1, j)
        (i - 1, j + 1)
        (i, j - 1)
        (i, j + 1)
        (i + 1, j - 1)
        (i + 1, j)
        (i + 1, j + 1)
    }


let rec number_indices_impl idx (line: char array) =

    let non_digits = line |> Array.skipWhile (Char.IsAsciiDigit >> not)

    let amount_skipped = line.Length - non_digits.Length

    let digits = non_digits |> Array.takeWhile System.Char.IsAsciiDigit

    let len = amount_skipped + digits.Length

    let rest = line |> Array.skip len


    if Array.isEmpty digits then
        []
    else
        let remaining_results = (number_indices_impl (idx + len) rest)

        let number = digits |> String |> int

        // return the parsed number, and all indices at which it appears
        (number, [ idx + amount_skipped .. idx + len - 1 ]) :: remaining_results


let number_indices line =
    number_indices_impl 0 (Array.ofSeq line)


let solve (input: string array) =

    let number_indices = input |> Array.map number_indices

    let mutable sum = 0

    let mutable gears = Map.empty

    for i, number_indices in number_indices |> Array.indexed do
        for number, indices in number_indices do
            let all_siblings =
                indices
                |> List.map (sibling_indices i)
                |> Seq.concat
                |> Set.ofSeq
                |> Seq.map (fun (i, j) ->
                    match Array.tryItem i input |> Option.bind (Array.ofSeq >> Array.tryItem j) with
                    | Some '.' -> false
                    | Some ch when Char.IsAsciiDigit(ch) -> false
                    | Some '*' ->
                        gears <-
                            gears
                            |> Map.change (i, j) (function
                                | None -> Some [ number ]
                                | Some other -> Some(number :: other))
                        true
                    | Some _ -> true
                    | None -> false)
                |> Seq.exists id

            if all_siblings then sum <- sum + number else ()

    let part2 =
        gears
        |> Map.values
        |> Seq.filter (List.length >> (=) 2)
        |> Seq.map (List.reduce (*))
        |> Seq.reduce (+)

    sum, part2


let part1 = solve >> fst
let part2 = solve >> snd

let example =
    """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
    """
    |> Utils.lines

assert (example |> part1 |> (=) 4361)
assert (example |> part2 |> (=) 467835)
