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
    [<Option("fg", Default = "(255, 255, 255)", HelpText = "Foreground Color")>] foregroundColor: string;
    [<Option("lower", Default = 33, HelpText = "Lower character (UTF-16) for font generation")>] lowerChar: int;
    [<Option("upper", Default = 126, HelpText = "Upper character (UTF-16) for font generation")>] upperChar: int
}

let createGeneratorSettings (cliOptions: CommandLineOptions): Result<GeneratorSettings, string> =
    let bgColor = parseColor cliOptions.backgroundColor
    match bgColor with
        | Some bgColor ->
            let fgColor = parseColor cliOptions.foregroundColor
            match fgColor with
                | Some fgColor ->
                    if File.Exists(cliOptions.fontFile) then
                        if cliOptions.upperChar > cliOptions.lowerChar then
                            let settings = { fontFileName = cliOptions.fontFile; outFileName = cliOptions.outputFileName; fontSize = cliOptions.fontSize; backgroundColor = bgColor; foregroundColor = fgColor; upperChar = cliOptions.upperChar; lowerChar = cliOptions.lowerChar }
                            Ok(settings)
                        else
                            Result.Error("The upper char must be bigger than the lower char")
                    else Result.Error(sprintf "Error: File %s does not exist" cliOptions.fontFile)
                | None -> Result.Error(sprintf "Error: Could not parse foreground color: %s" cliOptions.foregroundColor)
        | None -> Result.Error(sprintf "Error: Could not parse background color: %s" cliOptions.backgroundColor)

[<EntryPoint>]
let main argv =
    let result = CommandLine.Parser.Default.ParseArguments<CommandLineOptions>(argv)
    match result with
        | :? Parsed<CommandLineOptions> as parsed -> 
            match createGeneratorSettings parsed.Value with
                | Ok settings ->
                    match generate settings with
                        | Error e ->
                            Console.WriteLine(e)
                            1
                        | Ok ok ->
                            Console.WriteLine(ok)
                            printfn "Creating font successful. Font bitmap written to %s" (Path.GetFullPath(settings.outFileName))
                            0
                | Error err -> 
                    printfn "Invalid generator settings: %s" err
                    1
        | :? NotParsed<CommandLineOptions> as notParsed -> 
            printfn "Could not parse command line options, aborting."
            1
        | _ -> 2