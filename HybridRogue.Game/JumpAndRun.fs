module HybridRogue.Game.JumpAndRun

open Microsoft.Xna.Framework
open OpenSimplexNoise

type PickupItem =
    | Health of int

type Enemy = { health: int; radius: float32; position: Vector2; size: Vector2 }

let newEnemy health radius position size =
    { health = health; radius = radius; position = position; size = size }

type CollisionAction = 
    | Stop
    | NextLevel
    | AddItem of PickupItem

type StandingAction =
    | NoAction
    | Damage of int * int

type MountainLevelSettings = { waterLevel: int }

type UndergroundLevelSettings = { depth: int; lavaTreshold: float }

type LevelType = 
    | Underground of UndergroundLevelSettings
    | Mountain of MountainLevelSettings

type Block = { tileType: int; coordinates: int * int; color: Color; collisionAction: CollisionAction; standOnAction: StandingAction }

type Map = { sizeInTiles: Point; blocks: Block option array; startingPoint: Point; box: Rectangle }

type LevelPlayer = { position: Vector2; size: Vector2; velocity: Vector2 }

type Level = { map: Map; player: LevelPlayer; enemies: Enemy seq }

let tileSize = 16

type LevelParams = { size: Point; seed: int64; healthPickupPositions: int seq; levelType: LevelType; enemyPositions: int seq }

let levelParams (size: Point) (seed: int64) (healthPickupPositions: int seq) (enemies: int seq) (levelType: LevelType) =
    { size = size; seed = seed; healthPickupPositions = healthPickupPositions; levelType = levelType; enemyPositions = enemies }

let createPlayer (map: Map) =
    { position = map.startingPoint.ToVector2(); size = Vector2(float32(tileSize)); velocity = Vector2(0.0f, 0.0f) }

let generateLevel (param: LevelParams) =
    let mapSize = param.size.X * param.size.Y
    let blocks: Block option array = [| for i in 0..(mapSize - 1) -> None |]
    let mutable enemies = []
    match param.levelType with
        | Mountain mountainSettings ->
            let waterLevel = param.size.Y - mountainSettings.waterLevel
            let noise = OpenSimplexNoise(param.seed)
            let variation = float(param.size.Y / 2)
            for x = 0 to param.size.X - 2  do
                let nv = abs(int(noise.Evaluate(float(x) / 20.0, 0.0) * variation))
                let start = (param.size.Y - 2) - nv
                let sky = { tileType = 50; coordinates = (x, 0); color = Color.SkyBlue; collisionAction = Stop; standOnAction = StandingAction.NoAction }
                Array.set blocks x (Some(sky))
                for y = start to param.size.Y - 1 do
                    let index = (y * param.size.X) + x
                    let block = { tileType = 54; coordinates = (x, y); color = Color.White; collisionAction = Stop; standOnAction = StandingAction.NoAction }
                    Array.set blocks index (Some(block))
                for yw = waterLevel to start do
                    let index = (yw * param.size.X) + x
                    let waterBlock = { tileType = 46; coordinates = (x, yw); color = Color.Blue; collisionAction = Stop; standOnAction = StandingAction.NoAction }
                    Array.set blocks index (Some(waterBlock))
                if Seq.contains x param.healthPickupPositions then
                    let pickup = { tileType = 10; coordinates = (x, start - 1); color = Color.Green; collisionAction = AddItem(Health(5)); standOnAction = StandingAction.NoAction }
                    Array.set blocks (((start - 1) * param.size.X) + x) (Some(pickup))
                if Seq.contains x param.enemyPositions then
                    let enemy = { health = 10; radius = 5.0f; position = Vector2(float32(x), float32(start * tileSize)); size = Vector2(float32(tileSize)) }
                    enemies <- enemy :: enemies

            for y = 0 to param.size.Y - 1 do
                let index = (y * param.size.X) + param.size.X - 1
                let block = { tileType = 55; coordinates = (param.size.X - 1, y); color = Color.DarkGreen; collisionAction = NextLevel; standOnAction = StandingAction.NoAction }
                Array.set blocks index (Some(block))
        | Underground undergroundSettings ->
            let noise = OpenSimplexNoise(param.seed)
            for x = 0 to param.size.X - 1 do
                let ceiling = { tileType = 54; coordinates = (x, 0); color = Color.White; collisionAction = Stop; standOnAction = StandingAction.NoAction }
                Array.set blocks x (Some(ceiling))
                let last = (param.size.Y - 1)
                let blockValue = noise.Evaluate(float(x) / 5.0, 0.0)
                let createBlock x y =
                    if blockValue > undergroundSettings.lavaTreshold then
                        { tileType = 46; coordinates = (x, y); color = Color.DarkOrange; collisionAction = Stop; standOnAction = StandingAction.Damage(1, 6) }
                    else
                        { tileType = 54; coordinates = (x, y); color = Color.White; collisionAction = Stop; standOnAction = StandingAction.NoAction }
                for y = (last - undergroundSettings.depth) + 1 to last do
                    let block = createBlock x y
                    let index = (y * param.size.X) + x
                    Array.set blocks index (Some(block))
                let start = (last - undergroundSettings.depth) + 1
                if Seq.contains x param.enemyPositions then
                    let enemy = { health = 10; radius = 5.0f; position = Vector2(float32(x), float32(start * tileSize)); size = Vector2(float32(tileSize)) }
                    enemies <- enemy :: enemies
                if Seq.contains x param.healthPickupPositions then
                    let pickup = { tileType = 10; coordinates = (x, start - 1); color = Color.Green; collisionAction = AddItem(Health(5)); standOnAction = StandingAction.NoAction }
                    Array.set blocks (((start - 1) * param.size.X) + x) (Some(pickup))

    let map = { sizeInTiles = param.size; blocks = blocks; startingPoint = Point(0, 300); box = Rectangle(0, 0, param.size.X * tileSize, param.size.Y * tileSize) }
    let player = createPlayer map
    { player = player; map = map; enemies = enemies }

let getBlock (map: Map) (x: int) (y: int) =
    let index = (map.sizeInTiles.X * y) + x
    if index > 0 && index < (Seq.length map.blocks) then
        Array.item index map.blocks
    else
        None

let mapIteri (iter: int -> int -> Block option -> unit) (map: Map) =
    for i = 0 to (Seq.length map.blocks) - 1 do
        let x = i % map.sizeInTiles.X
        let y = i / map.sizeInTiles.Y
        let block = Array.item i map.blocks
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