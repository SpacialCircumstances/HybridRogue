module HybridRogue.Game.GameMap

open Microsoft.Xna.Framework

type PickupItem =
    | Health of int

type CollisionAction = 
    | Stop
    | NextLevel
    | AddItem of PickupItem

type StandingAction =
    | NoAction
    | Damage of int * int

type Enemy = { health: int; radius: float32; position: Vector2; size: Vector2 }

type Block = { tileType: int; position: Vector2; color: Color; collisionAction: CollisionAction; standingAction: StandingAction }

type GameObject =
    | Block of Block
    | Enemy of Enemy
    | NoObject

type GameObjectStore = ResizeArray<GameObject>

let createGameObjectStore (initialSize: int): GameObjectStore =
    //TODO: BETTER STORAGE
    ResizeArray(initialSize)
    
let iterObjects (store: GameObjectStore) (iter: GameObject -> unit) =
    for obj in store do iter obj

let addObject (store: GameObjectStore) (obj: GameObject) =
    store.Add(obj)
    (store, store.Count - 1)

let removeObject (store: GameObjectStore) (obj: GameObject) =
    let index = store.IndexOf(obj)
    store.Item index <- NoObject
    store

type BlockMap = { size: Point; tileSize: int; blocks: int array; store: GameObjectStore; box: Rectangle }

let createBlockMap (store: GameObjectStore) (size: Point) =
    let blocks = Array.create (size.X * size.Y) -1
    let boxSize = Point(size.X * 16, size.Y * 16)
    { size = size; tileSize = 16; blocks = blocks; store = store; box = Rectangle(Point(), boxSize) }

let retrieveObject (store: GameObjectStore) (index: int) = store.Item index

let removeBlock (map: BlockMap) (tileX: int) (tileY: int) =
    //TODO: REMOVE FROM STORE?
    Array.set map.blocks ((map.size.X * tileY) + tileX) -1
    map

let setBlock (map: BlockMap) (tileX: int) (tileY: int) (block: Block) =
    let index = (map.size.X * tileY) + tileX
    let (_, objIndex) = addObject map.store (Block(block))
    Array.set map.blocks index objIndex
    map

let getBlock (map: BlockMap) (tileX: int) (tileY: int) =
    let index = (map.size.X * tileY) + tileX
    let item = Array.item index map.blocks
    if item = -1 then
        NoObject
    else
        retrieveObject map.store index

let blockAt (map: BlockMap) (pos: Vector2) =
    if map.box.Contains(pos) then
        let tileX = int(pos.X) / map.tileSize
        let tileY = int(pos.Y) / map.tileSize
        (tileX, tileY, getBlock map tileX tileY)
    else
        (-1, -1, NoObject)

let blockPosition (map: BlockMap) (tileX: int) (tileY: int) =
    (map.box.Location + Point(tileX * map.tileSize, tileY * map.tileSize)).ToVector2()