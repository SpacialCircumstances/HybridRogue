module HybridRogue.Game.Level

open Microsoft.Xna.Framework
open HybridRogue.Game.GameMap

type MountainLevelSettings = { waterLevel: int }

type UndergroundLevelSettings = { depth: int; lavaTreshold: float }

type LevelPlayer = { position: Vector2; size: Vector2; velocity: Vector2 }

type LevelType = 
    | Underground of UndergroundLevelSettings
    | Mountain of MountainLevelSettings

type LevelParams = { size: Point; seed: int64; healthPickupPositions: int seq; levelType: LevelType; enemyPositions: int seq }

type Level = { map: BlockMap; objects: GameObjectStore; player: LevelPlayer; enemies: Enemy seq }

let levelParams (size: Point) (seed: int64) (healthPickupPositions: int seq) (enemies: int seq) (levelType: LevelType) =
    { size = size; seed = seed; healthPickupPositions = healthPickupPositions; levelType = levelType; enemyPositions = enemies }

let generateLevel (param: LevelParams) =
    let store = createGameObjectStore (Seq.length param.enemyPositions + Seq.length param.healthPickupPositions + (param.size.X * param.size.Y) + 1) //Estimate size
    let blockMap = createBlockMap store param.size

    ()