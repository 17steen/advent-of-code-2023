module AdventOfCode2023.Days.Day08

open System
open AdventOfCode2023
open FSharp.Collections.ParallelSeq
open FSharp.Text.RegexProvider

type LineRegex = Regex< @"^(?<node>[A-Z]+) = \((?<left>[A-Z]+), (?<right>[A-Z]+)\)$" >

let example =
    """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
    """
        .Trim()

type NodeDefinition =
    { Start: int
      Left: int
      Right: int }

type Direction =
    | Left
    | Right

let hash_id (id: string) = 
    let chars = Array.ofSeq id 
    
    assert (chars.Length = 3)
    
    Array.fold (fun acc ch -> acc <<< 8 ||| (int ch)) 0 chars

let parse_line line =
    let ``match`` = LineRegex().TryTypedMatch(line) |> Option.get

    printfn "%A -> %A %A" ``match``.node.Value ``match``.left.Value ``match``.right.Value

    { Start = ``match``.node.Value |> hash_id
      Left = ``match``.left.Value |> hash_id
      Right = ``match``.right.Value |> hash_id }
      
    
let unhash_id hash =
    [|
        hash >>> 16 &&& 0xFF |> char
        hash >>> 8 &&& 0xFF |> char
        hash >>> 0 &&& 0xFF |> char
    |] |> String


let parse_directions =
    Array.ofSeq
    >> Array.map (function
        | 'R' -> Right
        | 'L' -> Left
        | invalid -> failwith $"invalid direction: {invalid}")
    
let map_node map node =
    Map.add node.Start (node.Left, node.Right) map

let parse (input: string) =
    let first, rest = input |> _.Split("\n\n") |> Utils.two_of

    let first = parse_directions first

    let rest = 
        rest 
        |> Utils.lines 
        |> Array.map parse_line
        |> Array.fold map_node Map.empty

    first, rest


let next_node nodes current_node direction = 
    let left, right = Map.find current_node nodes
    match direction with
    | Left -> left
    | Right -> right
    
let cycle_infinitely arr =
    seq {
        while true do
            yield! arr
   }
   
[<TailCall>]
let rec count_distance_impl  nodes current_count current_node node_to_reach directions =
    if current_node = node_to_reach then current_count else

    let direction = Seq.head directions
    
    
    let next_node = next_node nodes current_node direction
    
    count_distance_impl nodes (current_count + 1)  next_node node_to_reach (Seq.tail directions)

let count_distance nodes start_node node_to_reach directions =
    count_distance_impl nodes 0 (hash_id start_node) (hash_id node_to_reach) directions



let solve (input: string) = 

    let (directions, nodes) = parse input

    
    let directions = cycle_infinitely directions
    
    
    
    // nodes |> Map.iter (fun key (left, right) -> printfn "%A -> %A %A" (unhash_id key) (unhash_id left) (unhash_id right))
    
    nodes 
    |> Map.containsKey (hash_id "ZZZ")
    |> printfn "%A"

    let part1 = count_distance nodes "AAA" "ZZZ" directions
    
    (part1, 0)


