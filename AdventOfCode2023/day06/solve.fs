module AdventOfCode2023.Days.Day06

open System
open AdventOfCode2023
open FSharp.Collections.ParallelSeq

let example =
    """
Time:      7  15   30
Distance:  9  40  200
    """
        .Trim()

type Race = { Time: int64; Distance: int64 }

let parse input =
    let result =
        input
        |> Utils.lines
        |> Array.map Utils.space_split
        |> Array.map Array.tail
        |> Array.map (Array.map int64)

    let make_race a b = { Time = a; Distance = b }

    Array.map2 make_race result[0] result[1]

let parse_bad_kerning input =
    let result =
        input
        |> Utils.lines
        |> Array.map (Utils.space_split >> Array.tail >> String.concat "" >> int64)
        
    { Time = result[0]; Distance = result[1] }
    

let valid_race_options race =
    seq {
        for hold_time in 1L .. race.Time do
            let remaining_time = race.Time - hold_time

            if remaining_time * hold_time > race.Distance then
                yield hold_time
    }


let part1 = parse >> Array.map (valid_race_options >> Seq.length) >> Array.fold (*) 1
let part2 input =
    input
    |> parse_bad_kerning
    |> valid_race_options
    |> PSeq.length