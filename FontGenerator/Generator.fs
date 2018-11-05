module Generator

open Color
open System.IO
open System.Collections.Generic
open SixLabors.Primitives
open SixLabors.Fonts

type GeneratorSettings = {
    fontFileName: string;
    outFileName: string;
    fontSize: int;
    backgroundColor: RgbColor;
    foregroundColor: RgbColor
}

let bitmapMaxWidth = 1024
let lowerChar = 33
let upperChar = 127

let generate (settings: GeneratorSettings): Result<string, string> =
    let fontCollection = FontCollection()
    let font = fontCollection.Install(File.OpenRead(settings.fontFileName))
    
    let mutable bitmapWidth = 0
    let mutable bitmapHeight = 0

    let glyphs = Dictionary()

    for i = 33 to 127 do
        let newWidth = bitmapWidth + 64
        let character = char(i)
        glyphs.Add(character, Point(bitmapWidth, bitmapHeight))
        if newWidth > bitmapMaxWidth then
            bitmapWidth <- 64
            bitmapHeight <- bitmapHeight + 64
        else
            bitmapWidth <- newWidth    

    Error("Not Implemented")