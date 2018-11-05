// Learn more about F# at http://fsharp.org

open System
open CommandLine
open CommandLine.Text

type CommandLineOptions = {
    [<Value(0, MetaName = "font", HelpText = "Font file")>] fontFile: string;
    [<Option("size", HelpText = "Font size", Default = 64)>] fontSize: int;
    [<Option("out", Required = true, HelpText = "Image file output name")>] outputFileName: string;
    [<Option("bg", Default = "(0,0,0)", HelpText = "Background Color")>] backgroundColor: string;
    [<Option("fg", Default = "(255, 255, 255)", HelpText = "Foreground Color")>] foregroundColor: string
}

[<EntryPoint>]
let main argv =
    let result = CommandLine.Parser.Default.ParseArguments<CommandLineOptions>(argv)
    match result with
        | :? Parsed<CommandLineOptions> as parsed -> 
            
            0
        | :? NotParsed<CommandLineOptions> as notParsed -> 1
        | _ -> 2