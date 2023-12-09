module AdventOfCode2023.Utils

open System


let two_of array =
    assert (Array.length array = 2)
    array[0], array[1]

let lines (input: string) : string array = input.Trim().Split("\n")

let space_split (input: string) =
    input.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
