module AdventOfCode2023.Days.Day09

open System
open AdventOfCode2023
open FSharp.Collections.ParallelSeq
open FSharp.Text.RegexProvider

let parse_line = 
    Utils.space_split >> Array.map int64
    
let parse = Utils.lines >> Array.map parse_line

let differences array =
    Array.pairwise array
    |> Array.map (fun (a, b) -> b - a)

let rec extrapolate input  =
    let differences = differences input
    
    
    if Array.forall ((=) LanguagePrimitives.GenericZero) differences then
        Array.last input
    else 
        let extrapolated = 
            extrapolate differences
        Array.last input + extrapolated 

let rec extrapolate_back input  =

    let differences = differences input
    
    if Array.forall ((=) LanguagePrimitives.GenericZero) differences then
        Array.head input
    else 
        let extrapolated = 
            extrapolate_back differences
        Array.head input - extrapolated 
        
let example = 
    """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
    """.Trim()
    
let test = 
    
    let first = parse example |> Array.head
    
    extrapolate first


let pass_through func value = 
    func value
    value

let solve (input: string) =

    let parsed = parse input
    let part1 = parsed |> Array.map extrapolate |> Array.sum
    let part2 = parsed |> Array.map extrapolate_back |> Array.sum
    
    
    (part1, part2)


