module WebApp.Main

open System
open System.Net.Http
open System.Net.Http.Json
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Bolero.Html

open AdventOfCode2023

/// The Elmish application's model.
type Model =
    {
        day: int
        input: string
        results: Node
    }
    
    

let make_result_node part1 part2 =
    ul {
        li {
            $"Part1: %A{part1}"
        }
        li {
            $"Part2: %A{part2}"
        }
    }
    

let from_pair solver input =
    let part1, part2 = solver input
    make_result_node part1 part2
    
let from_parts part1 part2 input =
    make_result_node (part1 input) (part2 input)
    
let from_parts_lines part1 part2 = Utils.lines >> from_parts part1 part2

let days: (string -> Node) array = [|
        from_parts_lines Days.Day01.part1 Days.Day01.part2
        from_parts_lines Days.Day02.part1 Days.Day02.part2
        from_parts_lines Days.Day03.part1 Days.Day03.part2
        from_parts_lines Days.Day04.part1 Days.Day04.part2
        from_pair Days.Day05.solve
        from_parts Days.Day06.part1 Days.Day06.part2
    |]

let init =
    {
        day = Array.length days
        input = ""
        results = empty()
    }


type Message =
    | SetDay of int
    | Compute
    | Input of string

let update message model =
    match message with
    | Compute ->
        printfn "HELP"
        { model with results = days[model.day - 1] model.input}

    | SetDay day -> { model with day = day }
    | Input input -> { model with input = input }

type Main = Template<"wwwroot/main.html">

let make_day_options selected_day =
    concat {
        for day in 1 .. days.Length do
            option {
                attr.selected(day = selected_day)
                $"Day %i{day}"
            }
    }

let view (model: Model) dispatch =
    Main()
        .Input(model.input, dispatch << Input)
        .Days(make_day_options model.day)
        .Compute(fun _ -> dispatch Compute)
        .Results(model.results)
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override _.CssScope = CssScopes.MyApp

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        Program.mkSimple (fun _ -> init) update view