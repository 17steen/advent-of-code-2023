module AdventOfCode2023.Days.Day05

open System
open AdventOfCode2023

open FSharp.Collections.ParallelSeq
open FSharp.Text.RegexProvider

type SeedsRegex = Regex< @"seeds:\s+(?<seeds>.*)$" >
type FirstLineRegex = Regex< @"^(?<from>[a-z]+)-to-(?<to>[a-z]+) map:$" >

type Range =
    { Source: int64
      Destination: int64
      Length: int64 }

type DescriptionMap =
    { From: string
      To: string
      Ranges: Range array }

let inline map_destination_location_impl (location: int64) (range: Range) =
    if location >= range.Destination && location < range.Destination + range.Length then
        location - range.Destination + range.Source |> Some
    else
        None

let inline map_destination_location (location: int64) (ranges: Range array) : int64 =
    ranges
    |> Array.tryPick (map_destination_location_impl location)
    |> Option.defaultValue location

let map_source_location_impl (location: int64) (range: Range) =
    if location >= range.Source && location < range.Source + range.Length then
        location - range.Source + range.Destination |> Some
    else
        None

let map_source_location (location: int64) (ranges: Range array) : int64 =
    ranges
    |> Array.tryPick (map_source_location_impl location)
    |> Option.defaultValue location

let parse_seeds line =
    SeedsRegex().TypedMatch(line).seeds.Value
    |> Utils.space_split
    |> Array.map int64

let parse_range line : Range =
    let as_ints = line |> Utils.space_split |> Array.map int64

    assert (as_ints.Length = 3)

    { Source = as_ints[1]
      Destination = as_ints[0]
      Length = as_ints[2] }

let parse_first_line line =
    let result = FirstLineRegex().TypedMatch line

    {| From = result.from.Value
       To = result.``to``.Value |}

let parse_description_map (input: string) : DescriptionMap =
    let splat = input.Split("\n")

    let description = splat[0] |> parse_first_line

    let ranges = splat[1..] |> Array.map parse_range

    { From = description.From
      To = description.To
      Ranges = ranges }


let parse (input: string) =
    let splat =
        input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

    let seeds = splat |> Array.head |> parse_seeds
    

    let seed_ranges =
        seeds
        |> Array.chunkBySize 2
        |> Array.map (fun arr -> {| Start = arr[0]; Length = arr[1] |})


    let descriptions = splat |> Array.tail |> Array.map parse_description_map

    {| Seeds = seeds
       SeedRanges = seed_ranges
       Descriptions = descriptions |}


let solve input =
    let parsed = parse input

    let seeds = parsed.Seeds

    let ranges_array = parsed.Descriptions |> Array.map (_.Ranges)
    
    let ranges_array_reversed = ranges_array |> Array.rev

    let locations =
        seeds
        |> Array.map (fun seed -> Array.fold map_source_location seed ranges_array)
        
    let inline is_location_contained_in_seed_ranges location =
       let source_location =
           Array.fold map_destination_location location ranges_array_reversed
           
       parsed.SeedRanges
       |> Array.exists (fun range -> source_location >= range.Start && source_location < range.Start + range.Length)
       
       
    let max = parsed.SeedRanges |> Array.map (fun range -> range.Start + range.Length) |> Array.max
    
    let part1 = locations |> Array.min
    let part2 = seq { 0L..max } |> PSeq.find is_location_contained_in_seed_ranges
       
    (part1 |> string, part2 |> string)