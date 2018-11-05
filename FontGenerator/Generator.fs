module Generator

open Color
open System.IO
open System.Collections.Generic
open SixLabors.Primitives
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.Shapes
open System

type GeneratorSettings = {
    fontFileName: string;
    outFileName: string;
    fontSize: int;
    backgroundColor: RgbColor;
    foregroundColor: RgbColor
}

let tileSize = 64
let bitmapMaxWidth = tileSize * 16
let lowerChar = 33
let upperChar = 126 //We do not want the DEL character

let generate (settings: GeneratorSettings): Result<string, string> =
    let fontCollection = FontCollection()
    let fontFamily = fontCollection.Install(File.OpenRead(settings.fontFileName))
    let font = fontFamily.CreateFont(float32(settings.fontSize))

    let mutable bitmapWidth = 0
    let mutable bitmapHeight = 0

    let glyphs = Dictionary()

    for i = lowerChar to upperChar do
        let newWidth = bitmapWidth + tileSize
        let character = char(i)
        glyphs.Add(character, PointF(float32(bitmapWidth), float32(bitmapHeight)))
        if newWidth >= bitmapMaxWidth then
            bitmapWidth <- 0
            bitmapHeight <- bitmapHeight + tileSize
        else
            bitmapWidth <- newWidth
            
    let image = new Image<Rgba32>(bitmapMaxWidth, bitmapHeight + tileSize)

    for entry in glyphs do
        let character = entry.Key
        let pos = entry.Value
        do image.Mutate(fun ctx -> 
            try
                ctx.DrawText(string(character), font, Pen(Rgba32.White, 1.0f), pos) |> ignore
            with
                | :? ImageProcessingException -> ())

    image.Save(settings.outFileName)

    Error("Not Implemented")