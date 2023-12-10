
let input = System.Console.In.ReadToEnd() |> _.Trim()
let result = AdventOfCode2023.Days.Day09.solve input


printfn "%A" result