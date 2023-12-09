module AdventOfCode2023.Days.Day09

open System
open AdventOfCode2023
open FSharp.Collections.ParallelSeq
open FSharp.Text.RegexProvider


let parse_line = 
    Utils.space_split >> Array.map int
    
let parse = Utils.lines >> Array.map parse_line


let solve (input: string) = 

    let parsed = parse input
    
    
    
    // nodes |> Map.iter (fun key (left, right) -> printfn "%A -> %A %A" (unhash_id key) (unhash_id left) (unhash_id right))
    
    nodes 
    |> Map.containsKey (hash_id "ZZZ")
    |> printfn "%A"

    let part1 = count_distance nodes "AAA" "ZZZ" directions
    
    (part1, 0)


