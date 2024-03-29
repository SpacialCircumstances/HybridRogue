﻿module Generator

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
    foregroundColor: RgbColor;
    lowerChar: int;
    upperChar: int;
    tileSize: int
}

let generate (settings: GeneratorSettings): Result<string, string> =
    try 
        let tileSize = settings.tileSize
        let bitmapMaxWidth = tileSize * 16
        let lowerChar = settings.lowerChar
        let upperChar = settings.upperChar
        let fontCollection = FontCollection()
        let fontFamily = fontCollection.Install(File.OpenRead(settings.fontFileName))
        let font = fontFamily.CreateFont(float32(settings.fontSize))

        let foregroundColor = toRgba32 settings.foregroundColor
        let backgroundColor = toRgba32 settings.backgroundColor

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

        do image.Mutate(fun ctx -> ctx.Fill(backgroundColor) |> ignore)

        let mutable errors = List.empty<string>

        for entry in glyphs do
            let character = string(entry.Key)
            let pos = entry.Value
            let size = TextMeasurer.Measure(character, RendererOptions(font, pos))
            let newPos = if size.Width > float32(tileSize) || size.Height > float32(tileSize) then
                                do errors <- (sprintf "Warning: Character %s with size (%f, %f) exceeds tile size %i" character size.Width size.Height tileSize) :: errors
                                pos
                            else
                                let tileSize = float32(tileSize)
                                let ow = (tileSize - size.Width) / 2.0f
                                let oh = (tileSize - size.Height) / 2.0f
                                PointF(pos.X + ow, pos.Y + oh)
            do image.Mutate(fun ctx -> 
                try
                    ctx.DrawText(character, font, foregroundColor, newPos) |> ignore
                with
                    | :? ImageProcessingException -> 
                        do errors <- (sprintf "Warning: Problem drawing character %s: Most likely, the font does not contain this character" character) :: errors
            )

        image.Save(settings.outFileName)
        Ok(List.fold (fun s e -> s + e + "\n") "" errors)    
    with
        | e -> Error(e.Message)
