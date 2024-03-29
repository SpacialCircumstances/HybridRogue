﻿module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Input
open GameMap
open Level
open Camera
open Microsoft.Xna.Framework.Graphics
open HybridRogue.Game
open System

let gravity = Vector2(0.0f, 0.16f)

type Damage = { elapsed: TimeSpan; damagePerSecond: int; countdown: int }

type Player = { name: string; level: int; levelQueue: LevelParams list; health: int; damage: Damage option }

let random = Random()

let nextRandom () = int64(random.Next())

let defaultLevels = [ 
    levelParams (Point(250, 40)) (nextRandom ()) [10; 200] [
        activeObjectParam 40 Bomb;
        activeObjectParam 50 Bomb;
        activeObjectParam 60 Bomb;
        activeObjectParam 67 Bomb;
        activeObjectParam 75 PoisonBomb;
        activeObjectParam 190 Bomb;
        activeObjectParam 210 Bomb;
        activeObjectParam 216 Bomb;
    ] (Mountain({ waterLevel = 4 }));
    levelParams (Point(120, 40)) (nextRandom ()) [50; 60; 110] [
        activeObjectParam 15 PoisonBomb;
        activeObjectParam 25 PoisonBomb;
        activeObjectParam 30 Bomb;
        activeObjectParam 65 PoisonBomb;
        activeObjectParam 82 Bomb;
        activeObjectParam 100 PoisonBomb;
    ] (Mountain({ waterLevel = 5 }));
    levelParams (Point(80, 40)) (nextRandom ()) [20; 30; 50] [
        activeObjectParam 12 PoisonBomb;
        activeObjectParam 38 Bomb;
        activeObjectParam 44 Bomb;
        activeObjectParam 60 PoisonBomb;
        activeObjectParam 69 PoisonBomb;
    ] (Mountain({ waterLevel = 7 }));
    levelParams (Point(140, 40)) (nextRandom()) [100; 110; 120; 130] [
        activeObjectParam 20 Bomb;
        activeObjectParam 31 Bomb;
        activeObjectParam 47 PoisonBomb;
        activeObjectParam 60 Bomb;
        activeObjectParam 69 Bomb;
        activeObjectParam 89 PoisonBomb;
        activeObjectParam 116 Bomb;
    ] (Mountain({ waterLevel = 1 }));
    levelParams (Point(100, 30)) (nextRandom ()) [40; 60; 80] [] (Underground({ depth = 5; lavaTreshold = 0.3 })) ]

let emptyPlayer = { name = "Player"; level = 1; levelQueue = defaultLevels; health = 50; damage = None }

type LevelState = { level: Level; player: Player; camera: Camera; timePlayed: TimeSpan }

type EndState = GameLost | GameFinished

type GameEndState = { player: Player; totalTimePlayed: TimeSpan; endState: EndState }

type GameState = 
        | MenuState
        | LevelState of LevelState
        | EndScreen of GameEndState

let initialGameState = MenuState

let emptyVec = Vector2(0.0f, 0.0f)

let normalVel = 2.0f
let maxVelC = 12.0f
let maxHealth = 50

let defaultCamera = createCamera (Vector2(0.0f, 300.0f)) 1.0f

let clamp minimum maximum value =
    max (min maximum value) minimum

let clampVelocity = clamp -maxVelC maxVelC
let clampHealth = clamp 0 maxHealth

let velocityByPressedKeys (oldVel: Vector2) (keyboard: KeyboardState) =
    let lpressed = keyboard.IsKeyDown(Keys.Left)
    let rpressed = keyboard.IsKeyDown(Keys.Right)
    if lpressed then
        if rpressed then
            Vector2(0.0f, oldVel.Y)
        else
            Vector2(-normalVel, oldVel.Y)
    else
        if rpressed then
            Vector2(normalVel, oldVel.Y)
        else
            Vector2(0.0f, oldVel.Y)

let calculateVelocity (onFloor: bool) (oldVel: Vector2) (event: InputEvent) =
    let vel = match event with
                | Pressed key ->
                    match key with
                        | Keys.Up -> 
                            if onFloor then Vector2(0.0f, -7.0f) + oldVel else velocityByPressedKeys oldVel (Keyboard.GetState())
                        | _ -> velocityByPressedKeys oldVel (Keyboard.GetState())
                | _ -> velocityByPressedKeys oldVel (Keyboard.GetState())
    let unclamped = if onFloor then vel else vel + gravity
    Vector2(clampVelocity unclamped.X, clampVelocity unclamped.Y)
   
let getTileBelow (map: BlockMap) (pos: Vector2) =
    let (x, y, b) = blockAt map pos
    match getBlock map x (y + 1) with
        | Block block -> Some(block)
        | _ -> None

let getFloor (map: BlockMap) (pos: Vector2) (size: Vector2) =
    OperationCombinators.orElse {
        return! getTileBelow map pos
        return! getTileBelow map (pos + size)
        return! getTileBelow map (Vector2(pos.X, pos.Y + size.Y))
        return! getTileBelow map (Vector2(pos.X + size.X, pos.Y))
    }

let updateActiveObjects (pos: Vector2) (aos: ActiveObjectHandle list) (getObject: ActiveObjectHandle -> GameObject) =
    let nearPlayerHandle = List.tryFind (fun o -> 
                                            match getObject o with
                                                | ActiveObject ao -> (pos - ao.position).Length() < ao.radius
                                                | _ -> false) aos

    match nearPlayerHandle with
        | Some handle -> let objectNearPlayer = match getObject handle with
                                                    | ActiveObject ao -> ao
                                                    | _ -> raise(InvalidOperationException())
                         (handle, Some(objectNearPlayer.radiusEnterAction))
        | None -> (-1, None)

let updatePlayer (player: Player) (levelPlayer: LevelPlayer) (standingAction: StandingAction) (activeObjects: ActiveObjectHandle list) (objects: GameObjectStore) (time: GameTime) =
    let (aoHandle, aoAction) = updateActiveObjects levelPlayer.position activeObjects (retrieveObject objects)
    let player = match aoAction with
                    | None -> player
                    | Some action -> 
                        removeObject objects aoHandle |> ignore //Very ugly solution because the ao is still present in the ao handle list
                        match action with
                            | Explosion dmg ->
                                { player with health = player.health - dmg }
                            | Poison (perSecond, duration) ->
                                { player with damage = Some({ elapsed = TimeSpan(); damagePerSecond = perSecond; countdown = duration })}
    match standingAction with
        | NoAction ->
            match player.damage with
                | None -> player
                | Some damage ->
                    let totalTime = damage.elapsed + time.ElapsedGameTime
                    let (newHealth, newDamage) = if totalTime.TotalSeconds > 1.0 then 
                                                        (player.health - damage.damagePerSecond,  if damage.countdown = 0 then None else
                                                                                                    Some({ damage with countdown = damage.countdown - 1; elapsed = totalTime - TimeSpan(0, 0, 1) }))
                                                 else (player.health, Some({ damage with elapsed = totalTime }))
                    { player with health = newHealth; damage = newDamage }
              
        | StandingAction.Damage (perSec, duration) ->
            match player.damage with
                | None -> 
                    let newDamage = { elapsed = TimeSpan(); damagePerSecond = perSec; countdown = duration }
                    { player with damage = Some(newDamage) }
                | Some dmg ->
                    let totalTime = dmg.elapsed + time.ElapsedGameTime
                    let (newHealth, newDamage) = if totalTime.TotalSeconds > 1.0 then
                                                        (player.health - dmg.damagePerSecond, 
                                                            { dmg with elapsed = totalTime - TimeSpan(0, 0, 1); countdown = max dmg.countdown duration; damagePerSecond = max dmg.damagePerSecond perSec })
                                                    else (player.health, { dmg with elapsed = totalTime; countdown = max dmg.countdown duration; damagePerSecond = max dmg.damagePerSecond perSec })
                                                        
                    { player with damage = Some(newDamage); health = newHealth } 

let addItem (item: PickupItem) (player: Player) =
    match item with
        | Health h ->
             { player with health = clampHealth (player.health + h) }

let updateLevel (level: Level) (player: Player) (camera: Camera) input standingAction timePlayed =
    let physicalPlayer = level.player
    let oldMap = level.map
    let onFloor = match (getFloor oldMap physicalPlayer.position physicalPlayer.size) with
                    | None -> false
                    | Some _ -> true
    let velocity = match input with
                        | Some input -> calculateVelocity onFloor physicalPlayer.velocity input
                        | None -> if onFloor then physicalPlayer.velocity else physicalPlayer.velocity + gravity
    let (oldx, oldy, _) = blockAt oldMap physicalPlayer.position
    let (bx, by, blockHit) = collisionCheck physicalPlayer.position physicalPlayer.size velocity oldMap
    match blockHit with
        | None -> 
            let newPosition = physicalPlayer.position + velocity
            let newPhysicalPlayer = { physicalPlayer with position = newPosition; velocity = velocity }
            let newCamera = { scale = camera.scale; position = newPhysicalPlayer.position }
            let newLevel = { level with player = newPhysicalPlayer }
            LevelState({ level = newLevel; player = player; camera = newCamera; timePlayed = timePlayed })
        | Some block ->
            match block.collisionAction with
                | Stop ->
                    let position = physicalPlayer.position
                    let xd = abs(bx - oldx)
                    let yd = abs(by - oldy)
                    let newPos = position + velocity
                    let (finalVel, finalPos) = if xd >= 1 then
                                                    if yd = 0 then
                                                        (Vector2(0.0f, velocity.Y), Vector2(position.X, newPos.Y))
                                                    else
                                                        (emptyVec, position)
                                                else
                                                    (Vector2(velocity.X, 0.0f), Vector2(newPos.X, position.Y))
                    let newPhysicalPlayer = { physicalPlayer with position = finalPos; velocity = finalVel }
                    let newCamera = { scale = camera.scale; position = newPhysicalPlayer.position }
                    let newLevel = { level with player = newPhysicalPlayer }
                    LevelState({ level = newLevel; player = player; camera = newCamera; timePlayed = timePlayed })
                | AddItem item -> 
                    let newPlayer = addItem item player
                    let newMap = unsetBlock oldMap bx by
                    let newLevel = { level with map = newMap }
                    LevelState({ level = newLevel; player = newPlayer; camera = camera; timePlayed = timePlayed })
                | CollisionAction.NextLevel -> 
                    let next = List.tryHead player.levelQueue
                    match next with
                        | Some nextLevel ->
                            let newLevel = generateLevel nextLevel
                            let newPlayer = { player with level = player.level + 1; levelQueue = player.levelQueue.Tail }
                            LevelState({ camera = defaultCamera; player = newPlayer; level = newLevel; timePlayed = timePlayed })
                        | None -> EndScreen({ player = player; totalTimePlayed = timePlayed; endState = GameFinished })

let updateState (state: GameState) (event: InputEvent option) (time: GameTime) =
    match state with
        | MenuState ->
            match event with
                | None -> state
                | Some event ->
                    match event with
                        | Released key ->
                            if key = Keys.Enter then
                                let player = emptyPlayer
                                let level = generateLevel player.levelQueue.Head
                                LevelState({ level =  level; player = { player with levelQueue = player.levelQueue.Tail }; camera = defaultCamera; timePlayed = TimeSpan() })
                            else
                                state
                        | _ -> state
        | LevelState levelState ->
            let player = levelState.level.player
            let map = levelState.level.map
            let playerCenter = Vector2(player.position.X + (player.size.X / 2.0f), player.position.Y + (player.size.Y / 2.0f))
            let standingAction = match (getTileBelow map playerCenter) with
                                    | None -> NoAction
                                    | Some tile -> tile.standingAction
            let gamePlayer = updatePlayer levelState.player levelState.level.player standingAction levelState.level.activeObjects levelState.level.objects time
            if gamePlayer.health <= 0 then
                EndScreen({ player = gamePlayer; totalTimePlayed = levelState.timePlayed + time.ElapsedGameTime; endState = GameLost })
            else
                updateLevel levelState.level gamePlayer levelState.camera event standingAction (levelState.timePlayed + time.ElapsedGameTime)
        | EndScreen endScreenState -> EndScreen(endScreenState)
                    

let drawPlayer (graphics: GraphicsState) (player: LevelPlayer) =
    let (texture, region) = getTile graphics.tileset 2
    let scale = player.size / float32(region.Size.X)
    do graphics.batch.Draw(texture, player.position, System.Nullable(region), Color.Blue, 0.0f, emptyVec, scale, SpriteEffects.None, 0.0f)             

let drawObjects (graphics: GraphicsState) (objects: GameObjectStore) =
    do iterObjects objects (fun o ->
        match o with
            | NoObject -> ()
            | ActiveObject activeObject ->
                let (texture, region) = getTile graphics.tileset activeObject.tileType
                graphics.batch.Draw(texture, activeObject.position, System.Nullable(region), activeObject.color,0.0f, emptyVec, Vector2(0.25f, 0.25f), SpriteEffects.None, 0.0f)
                //TODO: Draw radius
            | Block block ->
                let (texture, region) = getTile graphics.tileset block.tileType
                graphics.batch.Draw(texture, block.position, System.Nullable(region), block.color, 0.0f, emptyVec, Vector2(0.25f, 0.25f), SpriteEffects.None, 0.0f)
    )

let drawState (state: GameState) (graphics: GraphicsState) =
    let batch = graphics.batch
    match state with
        | MenuState ->
            batch.Begin()
            batch.DrawString(graphics.font, "Press Enter to start a new game", Vector2(0.0f, 300.0f), Color.White)
            batch.End()
        | LevelState state ->
            let transform = calculateTransform state.camera graphics.viewportSize
            batch.Begin(transformMatrix = System.Nullable(transform)) //Draw level
            drawObjects graphics state.level.objects
            drawPlayer graphics state.level.player
            batch.End()
            batch.Begin() //Draw gui
            let healthColor = match state.player.damage with
                                    | None -> Color.White
                                    | Some _ -> Color.Red
            batch.DrawString(graphics.font, (sprintf "Health %i" state.player.health), Vector2(0.0f, 50.0f), healthColor)
            batch.DrawString(graphics.font, (sprintf "Level %i" state.player.level), Vector2(0.0f, 0.0f), Color.White)
            batch.End()
        | EndScreen endScreenState ->
            batch.Begin()
            match endScreenState.endState with
                | GameFinished -> batch.DrawString(graphics.font, "Game Won!", Vector2(), Color.Blue)
                | GameLost -> batch.DrawString(graphics.font, "Game Over", Vector2(), Color.Red)
            batch.DrawString(graphics.font, (sprintf "Player: %s" endScreenState.player.name), Vector2(0.0f, 50.0f), Color.White)
            batch.DrawString(graphics.font, (sprintf "Level: %i" endScreenState.player.level), Vector2(0.0f, 100.0f), Color.White)
            batch.DrawString(graphics.font, (sprintf "Time: %s" (endScreenState.totalTimePlayed.ToString())), Vector2(0.0f, 150.0f), Color.White)
            batch.End()
    ()