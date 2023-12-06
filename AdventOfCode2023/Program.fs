module AdventOfCode2023.Program

open System.IO

open AdventOfCode2023.Days


let days = [|
    "day01/input", (Day01.part1, Day01.part2)
    "day02/input", (Day02.part1, Day02.part2)
    "day03/input", (Day03.part1, Day03.part2)
    "day04/input", (Day04.part1, Day04.part2)
|]
   
let day n =
    let file, (part1, part2) = Array.get days (n - 1)
    let input = File.ReadAllLines file |> Array.filter (_.Length >> (<>) 0)
    
    printfn $"Part1: %A{part1 input}"
    printfn $"Part2: %A{part2 input}"

day 4