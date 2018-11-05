module Color

open System.Text.RegularExpressions
open SixLabors.ImageSharp.PixelFormats

type RgbColor = { r: byte; g: byte; b: byte }

let colorRegex = Regex("\((\d{1,3}),\s?(\d{1,3}),\s?(\d{1,3})\)")

let parseColor (colorString: string): RgbColor option =
    let rmatch = colorRegex.Match colorString
    match rmatch.Success with
        | true ->
            let r = byte((rmatch.Groups.Item 1).Value)
            let g = byte((rmatch.Groups.Item 2).Value)
            let b = byte((rmatch.Groups.Item 3).Value)
            let color = { r = r; g = g; b = b }
            Some(color)
        | false ->
            None

let toRgba32 color =
    Rgba32(color.r, color.g, color.b)