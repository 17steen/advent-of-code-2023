module AdventOfCode2023.Days.Day04

open AdventOfCode2023

open FSharp.Text.RegexProvider

type LineRegex = Regex< @"^Card\s+(?<id>\d+): (?<winning>.*) \| (?<hand>.*)$" >

type Card =
    { id: int
      winning: int array
      hand: int array }

let parse line : Card =
    let parsed = LineRegex().TypedMatch(line)

    { id = parsed.id.Value |> int
      winning = parsed.winning.Value |> Utils.space_split |> Array.map int
      hand = parsed.hand.Value |> Utils.space_split |> Array.map int }

let winning_count card =
    card.hand
    |> Array.filter (fun v -> Array.contains v card.winning)
    |> Array.length


let points winning_count =
    if winning_count > 0 then
        2.0 ** (winning_count - 1 |> double) |> int
    else
        0


let process_cards (cards: Card array) =
    let winning_counts = cards |> Array.map winning_count

    let mutable map = Array.zeroCreate winning_counts.Length |> Array.map ((+) 1)

    for idx, wc in winning_counts |> Array.indexed do
        for i in 1..wc do
            let next_idx = idx + i
            map[next_idx] <- map[next_idx] + 1 * map[idx]

    Array.sum map


let part1 = Array.map (parse >> winning_count >> points) >> Array.sum
let part2 = Array.map parse >> process_cards

let example =
    """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

assert (example |> Utils.lines |> part1 |> (=) 13)
assert (example |> Utils.lines |> part2 |> (=) 30)
