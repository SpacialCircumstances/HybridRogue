module HybridRogue.Game.JumpAndRun

open Microsoft.Xna.Framework

type Block = { tileType: int; coordinates: int * int }

type Map = { size: Point; blocks: Block option seq }

type Level = { map: Map }

let tileSize = 16

let defaultLevel = 
    let blocks = [| for i in 0..25 -> Some({ tileType = (i % 5) + 10; coordinates = (i % 5, i / 5) }) |]
    { map = { size = Point(5, 5); blocks = blocks }}

let mapIteri (iter: int -> int -> Block option -> unit) (map: Map) =
    for i = 0 to (Seq.length map.blocks) - 1 do
        let x = i % map.size.X
        let y = i / map.size.Y
        let block = Seq.item i map.blocks
        do iter x y block