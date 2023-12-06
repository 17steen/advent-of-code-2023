module AdventOfCode2023.Days.Day02
open AdventOfCode2023

open FSharp.Text.RegexProvider

type GameRegex = Regex< @"^Game (?<id>\d+): (?<rest>.*)$" >

type CubeRegex = Regex< @"^(?<cube_count>\d+) (?<cube_color>\w+)$" >

type Subset = Map<string, int>

type Game =
    { id: int
      cubes: Subset array }

let parse_game line : Game =
    let game_match = GameRegex().TypedMatch(line)

    let cubes =
        game_match
        |> _.rest.Value.Split("; ")
        |> Array.map (
            _.Split(", ")
            >> (Array.fold
                    (fun map line ->
                        let matched = CubeRegex().TypedMatch(line)
                        Map.add (matched.cube_color.Value) (matched.cube_count.Value |> int) map)
                    Map.empty)
        )

    { id = (game_match.id.Value |> int)
      cubes = cubes }

let max_colors = Map [ ("red", 12); ("green", 13); ("blue", 14) ]

let can_be_played (max_colors: Subset) (game: Game) =

    let pred color count =
        let max =
            Map.find color max_colors
        count <= max

    game.cubes
    |> Array.forall (Map.forall pred)

let fewest_amount_of_cubes (game: Game) =
    
    let pick_max (value:int) (max: int option): int option =
        match max with
        | None -> Some value
        | Some old_max when old_max < value -> Some(value)
        | Some _ -> max
    
    let folder (fewest:Subset) (subset: Subset): Subset =
        Map.fold (fun state color count -> Map.change color (pick_max count) state) fewest subset
        
        
    game.cubes
    |> Array.fold folder Map.empty
    |> Map.values
    |> Seq.reduce (*)
    

let example =
    """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

let part1 (input: string array) =
    input
    |> Array.map parse_game
    |> Array.filter (can_be_played max_colors)
    |> Array.map _.id
    |> Array.reduce (+)
    
let part2 (input: string array) =
    input
    |> Array.map parse_game
    |> Array.map fewest_amount_of_cubes
    |> Array.reduce (+)
    
let test =
    example
    |> _.Trim()
    |> _.Split("\n")
    |> Array.map (parse_game >> fewest_amount_of_cubes)


printfn "%A" test

assert ((example |> Utils.lines |> part1) = 8)
assert ((example |> Utils.lines |> part2) = 2286)
