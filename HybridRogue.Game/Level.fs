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

let createCeiling map x y =
    { tileType = 54; position = blockPosition map x y; color = Color.White; collisionAction = Stop; standingAction = StandingAction.NoAction }

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
            for x = 0 to param.size.X - 1 do
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
        | Mountain mountainSettings ->
            ()
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
                | Block block -> (x - 1, y, Some(block))
                | _ ->
                    let (x, y, b) = blockAt (Vector2(newPos.X, newPos.Y + size.Y))
                    match b with
                        | Block block -> (x, y - 1, Some(block))
                        | _ ->
                            let (x, y, b) = blockAt (newPos + size)
                            match b with
                                | Block block -> (x - 1, y - 1, Some(block))
                                | _ -> (x - 1, y - 1, None)
