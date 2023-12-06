module AdventOfCode2023.Utils

open System


let lines (input: string) : string array = input.Trim().Split("\n")

let space_split (input: string) =
    input.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
