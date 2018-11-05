// Learn more about F# at http://fsharp.org

open System
open CommandLine
open CommandLine.Text
open Color
open Generator
open System.IO

type CommandLineOptions = {
    [<Value(0, MetaName = "font", HelpText = "Font file")>] fontFile: string;
    [<Option("size", HelpText = "Font size", Default = 64)>] fontSize: int;
    [<Option("out", Required = true, HelpText = "Image file output name")>] outputFileName: string;
    [<Option("bg", Default = "(0,0,0)", HelpText = "Background Color")>] backgroundColor: string;
    [<Option("fg", Default = "(255, 255, 255)", HelpText = "Foreground Color")>] foregroundColor: string
}

let createGeneratorSettings (cliOptions: CommandLineOptions): GeneratorSettings option =
    let bgColor = parseColor cliOptions.backgroundColor
    match bgColor with
        | Some bgColor ->
            let fgColor = parseColor cliOptions.foregroundColor
            match fgColor with
                | Some fgColor ->
                    if File.Exists(cliOptions.fontFile) then
                        let settings = { fontFileName = cliOptions.fontFile; outFileName = cliOptions.outputFileName; fontSize = cliOptions.fontSize; backgroundColor = bgColor; foregroundColor = fgColor }
                        Some(settings)
                    else None
                | None -> None
        | None -> None

[<EntryPoint>]
let main argv =
    let result = CommandLine.Parser.Default.ParseArguments<CommandLineOptions>(argv)
    match result with
        | :? Parsed<CommandLineOptions> as parsed -> 
            match createGeneratorSettings parsed.Value with
                | Some settings ->
                    match generate settings with
                        | Error e ->
                            Console.WriteLine(e)
                            1
                        | Ok ok ->
                            Console.WriteLine(ok)
                            0
                | None -> 1
        | :? NotParsed<CommandLineOptions> as notParsed -> 1
        | _ -> 2