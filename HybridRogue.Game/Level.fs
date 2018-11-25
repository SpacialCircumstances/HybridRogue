module HybridRogue.Game.Level

open Microsoft.Xna.Framework
open HybridRogue.Game.GameMap
open OpenSimplexNoise

type MountainLevelSettings = { waterLevel: int }

type UndergroundLevelSettings = { depth: int; lavaTreshold: float }

type UndergroundBlockType = Lava | Ground

type LevelPlayer = { position: Vector2; size: Vector2; velocity: Vector2 }

type LevelType = 
    | Underground of UndergroundLevelSettings
    | Mountain of MountainLevelSettings

type LevelParams = { size: Point; seed: int64; healthPickupPositions: int seq; levelType: LevelType; enemyPositions: int seq }

type Level = { map: BlockMap; objects: GameObjectStore; player: LevelPlayer; enemies: Enemy seq }

let levelParams (size: Point) (seed: int64) (healthPickupPositions: int seq) (enemies: int seq) (levelType: LevelType) =
    { size = size; seed = seed; healthPickupPositions = healthPickupPositions; levelType = levelType; enemyPositions = enemies }

let createNextLevelBlock map x y =
    { tileType = 55; position = blockPosition map x y; color = Color.Black; collisionAction = NextLevel; standingAction = NoAction }

let createCeiling map x y =
    { tileType = 54; position = blockPosition map x y; color = Color.White; collisionAction = Stop; standingAction = StandingAction.NoAction }

let createSky map x y =
    { tileType = 50; position = blockPosition map x y; color = Color.SkyBlue; collisionAction = Stop; standingAction = StandingAction.NoAction }

let createMountainBlock map x y =
    { tileType = 54; position = blockPosition map x y; color = Color.White; collisionAction = Stop; standingAction = StandingAction.NoAction }

let createWaterBlock map x y =
    { tileType = 46; position = blockPosition map x y; color = Color.Blue; collisionAction = Stop; standingAction = NoAction }

let createUndergroundBlock (blockType: UndergroundBlockType) map x y =
    let pos = blockPosition map x y
    match blockType with
        | Lava ->
            { tileType = 46; position = pos; color = Color.DarkOrange; collisionAction = Stop; standingAction = StandingAction.Damage(1, 6) }
        | Ground ->
            { tileType = 54; position = pos; color = Color.White; collisionAction = Stop; standingAction = StandingAction.NoAction }

let createHealthPickup map x y =
    let pos = blockPosition map x y
    { tileType = 10; position = pos; color = Color.Green; collisionAction = AddItem(Health(5)); standingAction = StandingAction.NoAction }

let generateLevel (param: LevelParams) =
    let store = createGameObjectStore (Seq.length param.enemyPositions + Seq.length param.healthPickupPositions + (param.size.X * param.size.Y) + 1) //Estimate size
    let blockMap = createBlockMap store param.size
    match param.levelType with
        | Underground undergroundSettings ->
            let noise = OpenSimplexNoise(param.seed)
            for x = 0 to param.size.X - 2 do
                let blockMap = setBlock blockMap x 0 (createCeiling blockMap x 0)
                let blockType = if noise.Evaluate(float(x) / 5.0, 0.0) > undergroundSettings.lavaTreshold then Lava else Ground
                let createBlock = createUndergroundBlock blockType blockMap
                let last = param.size.Y - 1
                for y = (last - undergroundSettings.depth) + 1 to last do
                    let block = createBlock x y
                    setBlock blockMap x y block |> ignore
                let start = last - undergroundSettings.depth
                if Seq.contains x param.healthPickupPositions then
                    setBlock blockMap x start (createHealthPickup blockMap x start) |> ignore
                //TODO: ENEMIES

            let last = param.size.X - 1
            for y = 0 to param.size.Y - 1 do
                setBlock blockMap last y (createNextLevelBlock blockMap last y) |> ignore
        | Mountain mountainSettings ->
            let noise = OpenSimplexNoise(param.seed)
            let waterLevel = param.size.Y - mountainSettings.waterLevel
            let variation = float(param.size.Y / 2)
            for x = 0 to param.size.X - 2 do
                let blockMap = setBlock blockMap x 0 (createSky blockMap x 0)
                let nv = abs(int(noise.Evaluate(float(x) / 20.0, 0.0) * variation))
                let start = (param.size.Y - 2) - nv
                for y = start to param.size.Y - 1 do
                    setBlock blockMap x y (createMountainBlock blockMap x y) |> ignore
                for y = waterLevel to start do
                    setBlock blockMap x y (createWaterBlock blockMap x y) |> ignore
                if Seq.contains x param.healthPickupPositions then
                    setBlock blockMap x start (createHealthPickup blockMap x start) |> ignore
            
            let last = param.size.X - 1
            for y = 0 to param.size.Y - 1 do
                setBlock blockMap last y (createNextLevelBlock blockMap last y) |> ignore

    let player = { position = Vector2(0.0f, 300.0f); size = Vector2(float32(blockMap.tileSize)); velocity = Vector2() }
    { map = blockMap; objects = store; player = player; enemies = [] }

let collisionCheck (position: Vector2) (size: Vector2) (velocity: Vector2) (map: BlockMap): (int * int * Block option) =
    let blockAt = blockAt map
    let newPos = position + velocity
    let (x, y, b) = blockAt (newPos)
    match b with
        | Block block -> (x, y, Some(block))
        | _ ->
            let (x, y, b) = blockAt (Vector2(newPos.X + size.X, newPos.Y))
            match b with
                | Block block -> (x, y, Some(block))
                | _ ->
                    let (x, y, b) = blockAt (Vector2(newPos.X, newPos.Y + size.Y))
                    match b with
                        | Block block -> (x, y, Some(block))
                        | _ ->
                            let (x, y, b) = blockAt (newPos + size)
                            match b with
                                | Block block -> (x, y, Some(block))
                                | _ -> (x, y, None)
