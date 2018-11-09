module HybridRogue.Game.JumpAndRun

open Microsoft.Xna.Framework
open OpenSimplexNoise

type Block = { tileType: int; coordinates: int * int }

type Map = { size: Point; blocks: Block option seq }

type Level = { map: Map; startingPoint: Point }

let tileSize = 16

type LevelParams = { size: Point; seed: int64 }

let generateLevel (param: LevelParams) =
    let mapSize = param.size.X * param.size.Y
    let blocks: Block option array = [| for i in 0..(mapSize - 1) -> None |]
    let noise = OpenSimplexNoise(param.seed)
    let variation = float(param.size.Y / 2)
    for x = 0 to param.size.X - 1  do
        let nv = abs(int(noise.Evaluate(float(x), 0.0) * variation))
        let start = (param.size.Y - 2) - nv
        for y = start to param.size.Y - 1 do
            let index = (y * param.size.X) + x
            let block = { tileType = 54; coordinates = (x, y) }
            Array.set blocks index (Some(block))

    { size = param.size; blocks = blocks }

let defaultLevel = 
    let map = generateLevel ({ size = Point(100, 40); seed = 12L })
    { map = map; startingPoint = Point(0, 300) }

let mapIteri (iter: int -> int -> Block option -> unit) (map: Map) =
    for i = 0 to (Seq.length map.blocks) - 1 do
        let x = i % map.size.X
        let y = i / map.size.Y
        let block = Seq.item i map.blocks
        do iter x y block    

