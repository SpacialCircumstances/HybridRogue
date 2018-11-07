module HybridRogue.Game.JumpAndRun

open Microsoft.Xna.Framework

type Block = { tileType: int }

type Map = { size: Point; blocks: Block option array }

type Level = { map: Map }

let defaultLevel = 
    let blocks = [| for i in 0..25 -> Some({ tileType = (i % 5) + 10 }) |]
    { map = { size = Point(5, 5); blocks = blocks }}