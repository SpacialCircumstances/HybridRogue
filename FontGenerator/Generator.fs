module Generator

open Color

type GeneratorSettings = {
    fontFileName: string;
    outFileName: string;
    fontSize: int;
    backgroundColor: RgbColor;
    foregroundColor: RgbColor
}