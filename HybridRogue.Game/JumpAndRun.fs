module HybridRogue.Game.JumpAndRun

open Microsoft.Xna.Framework
open OpenSimplexNoise

type CollisionAction = 
    | Stop
    | NextLevel

type LevelType = Underground | Mountain

type Block = { tileType: int; coordinates: int * int; color: Color; collisionAction: CollisionAction }

type Map = { sizeInTiles: Point; blocks: Block option seq; startingPoint: Point; box: Rectangle }

type LevelPlayer = { position: Vector2; size: Vector2; velocity: Vector2 }

type Level = { map: Map; player: LevelPlayer }

let tileSize = 16

type LevelParams = { size: Point; seed: int64; levelType: LevelType }

let levelParams (size: Point) (seed: int64) (levelType: LevelType) =
    { size = size; seed = seed; levelType = levelType }

let createPlayer (map: Map) =
    { position = map.startingPoint.ToVector2(); size = Vector2(float32(tileSize)); velocity = Vector2(0.0f, 0.0f) }

let generateLevel (param: LevelParams) =
    let mapSize = param.size.X * param.size.Y
    let blocks: Block option array = [| for i in 0..(mapSize - 1) -> None |]
    match param.levelType with
        | Mountain ->
            let noise = OpenSimplexNoise(param.seed)
            let variation = float(param.size.Y / 2)
            for x = 0 to param.size.X - 2  do
                let nv = abs(int(noise.Evaluate(float(x) / 20.0, 0.0) * variation))
                let start = (param.size.Y - 2) - nv
                let sky = { tileType = 50; coordinates = (x, 0); color = Color.SkyBlue; collisionAction = Stop }
                Array.set blocks x (Some(sky))
                for y = start to param.size.Y - 1 do
                    let index = (y * param.size.X) + x
                    let block = { tileType = 54; coordinates = (x, y); color = Color.White; collisionAction = Stop }
                    Array.set blocks index (Some(block))

            for y = 0 to param.size.Y - 1 do
                let index = (y * param.size.X) + param.size.X - 1
                let block = { tileType = 55; coordinates = (param.size.X - 1, y); color = Color.DarkGreen; collisionAction = NextLevel }
                Array.set blocks index (Some(block))
        | Underground ->
            let lastRow = (param.size.Y - 1) * param.size.X
            for x = 0 to param.size.X - 1 do
                let block = { tileType = 54; coordinates = (x, param.size.Y - 1); color = Color.White; collisionAction = Stop }
                Array.set blocks (x + lastRow) (Some(block))

    let map = { sizeInTiles = param.size; blocks = blocks; startingPoint = Point(0, 300); box = Rectangle(0, 0, param.size.X * tileSize, param.size.Y * tileSize) }
    let player = createPlayer map
    { player = player; map = map }

let getBlock (map: Map) (x: int) (y: int) =
    let index = (map.sizeInTiles.X * y) + x
    if index > 0 && index < (Seq.length map.blocks) then
        Seq.item index map.blocks
    else
        None

let mapIteri (iter: int -> int -> Block option -> unit) (map: Map) =
    for i = 0 to (Seq.length map.blocks) - 1 do
        let x = i % map.sizeInTiles.X
        let y = i / map.sizeInTiles.Y
        let block = Seq.item i map.blocks
        do iter x y block    

let tileRect (tileX: int) (tileY: int) =
    Rectangle(tileX * tileSize, tileY * tileSize, tileSize, tileSize)

let blockAt (map: Map) (pos: Vector2) =
    if map.box.Contains(pos) then
        let tileX = int(pos.X) / tileSize
        let tileY = int(pos.Y) / tileSize
        (tileX, tileY, getBlock map tileX tileY)
    else
        (-1, -1, None)

let clampToMapCoords (map: Map) (point: Point) =
    if map.box.Contains(point) then
        point
    else
        let x = max (min point.X map.box.Right) map.box.Left
        let y = max (min point.Y map.box.Bottom) map.box.Top
        Point(x, y)