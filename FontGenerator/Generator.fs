module Generator

open Color
open System.IO
open System.Collections.Generic
open SixLabors.Primitives
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing

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
    let fontFamily = fontCollection.Install(File.OpenRead(settings.fontFileName))
    let font = fontFamily.CreateFont(float32(settings.fontSize))

    let mutable bitmapWidth = 0
    let mutable bitmapHeight = 0

    let glyphs = Dictionary()

    for i = 33 to 127 do
        let newWidth = bitmapWidth + 64
        let character = char(i)
        glyphs.Add(character, PointF(float32(bitmapWidth), float32(bitmapHeight)))
        if newWidth > bitmapMaxWidth then
            bitmapWidth <- 64
            bitmapHeight <- bitmapHeight + 64
        else
            bitmapWidth <- newWidth
            
    let image = new Image<Rgba32>(bitmapWidth, bitmapHeight)

    for entry in glyphs do
        let character = entry.Key
        let pos = entry.Value
        do image.Mutate(fun ctx -> ctx.DrawText(string(character), font, Rgba32.White, pos) |> ignore)

    image.Save(settings.outFileName)

    Error("Not Implemented")