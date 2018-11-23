﻿module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Input
open JumpAndRun
open Camera
open Microsoft.Xna.Framework.Graphics
open HybridRogue.Game
open System
open HybridRogue.Game

let gravity = Vector2(0.0f, 0.15f)

type Damage = { elapsed: TimeSpan; damagePerSecond: int; countdown: int }

type Player = { name: string; level: int; levelQueue: LevelParams list; health: int; damage: Damage option }

let defaultLevels = [ 
    levelParams (Point(200, 40)) 15L [10] [20] (Mountain({ waterLevel = 5 }));
    levelParams (Point(100, 30)) 12L [40; 60; 80] [] (Underground({ depth = 5; lavaTreshold = 0.3 })) ]

let emptyPlayer = { name = "TestDummy"; level = 1; levelQueue = defaultLevels; health = 20; damage = None }

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
let maxVelC = 10.0f

let defaultCamera = createCamera (Vector2(0.0f, 300.0f)) 1.0f

let clamp minimum maximum value =
    max (min maximum value) minimum

let clampVelocity = clamp -maxVelC maxVelC

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
   
let getTileBelow (map: Map) (pos: Vector2) =
    let (x, y, b) = blockAt map pos
    getBlock map x (y + 1)

let getFloor (map: Map) (pos: Vector2) (size: Vector2) =
    OperationCombinators.orElse {
        return! getTileBelow map pos
        return! getTileBelow map (pos + size)
        return! getTileBelow map (Vector2(pos.X, pos.Y + size.Y))
        return! getTileBelow map (Vector2(pos.X + size.X, pos.Y))
    }

let updatePlayerEffects (player: Player) (standingAction: StandingAction) (time: GameTime) =
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



(*let updateLevelState (levelState: LevelState) (event: InputEvent option) (time: GameTime) =
    let player = levelState.level.player
    let map = levelState.level.map
    let camera = levelState.camera
    let floorTile = getFloor map player.position player.size
    let onFloor = match floorTile with
                    | None -> false
                    | Some _ -> true
    let playerVelocity = match event with
                            | Some event ->
                                calculateVelocity onFloor player.velocity event
                            | None -> player.velocity
    let unclampedVel = if onFloor then playerVelocity else playerVelocity + gravity
    let vel = Vector2(clampVelocity unclampedVel.X, clampVelocity unclampedVel.Y)
    let playerCenter = Vector2(player.position.X + (player.size.X / 2.0f), player.position.Y + (player.size.Y / 2.0f))
    let standingAction = match (getTileBelow map playerCenter) with
                            | None -> NoAction
                            | Some tile -> tile.standOnAction
    let gamePlayer = updatePlayerEffects levelState.player standingAction time
    if gamePlayer.health <= 0 then
        EndScreen({ player = gamePlayer; totalTimePlayed = levelState.timePlayed + time.ElapsedGameTime; endState = GameLost })
    else
        let collisionAction = collisionCheck player.position player.size vel map
        match collisionAction with
                | Move (pos, vel) ->
                    let newCamera = { scale = camera.scale; position = pos }
                    let newPlayer: LevelPlayer = { position = pos; size = player.size; velocity = vel }
                    let newLevel = { levelState.level with player = newPlayer }
                    LevelState({ camera = newCamera; player = gamePlayer; level = newLevel; timePlayed = levelState.timePlayed + time.ElapsedGameTime })
                | ChangePlayerAndMap (changePlayer, changeLevelPlayer, changeMap) ->
                    let newGamePlayer = changePlayer gamePlayer
                    let newLevelPlayer = changeLevelPlayer player
                    let newMap = changeMap map
                    let newCamera = { scale = camera.scale; position = newLevelPlayer.position }
                    let newLevel = { levelState.level with player = newLevelPlayer; map = newMap }
                    LevelState({ camera = newCamera; player = newGamePlayer; level = newLevel; timePlayed = levelState.timePlayed + time.ElapsedGameTime })
                | NextLevel ->
                    printfn "Reached end of level"
                    let newLevel = generateLevel levelState.player.levelQueue.Head
                    if List.isEmpty levelState.player.levelQueue then
                        EndScreen({ player = gamePlayer; totalTimePlayed = levelState.timePlayed + time.ElapsedGameTime; endState = GameFinished })
                    else
                        let newPlayer = { gamePlayer with level = levelState.player.level + 1; levelQueue = levelState.player.levelQueue.Tail; damage = None }
                        LevelState({ camera = defaultCamera; player = newPlayer; level = newLevel; timePlayed = levelState.timePlayed + time.ElapsedGameTime })
*)
let dummyColl pos size vel map = (-1, -1, None)

let updateLevel (level: Level) (player: Player) (camera: Camera) input standingAction timePlayed =
    let physicalPlayer = level.player
    let oldMap = level.map
    let onFloor = match (getFloor oldMap physicalPlayer.position physicalPlayer.size) with
                    | None -> false
                    | Some _ -> true
    let velocity = match input with
                        | Some input -> calculateVelocity onFloor physicalPlayer.velocity input
                        | None -> physicalPlayer.velocity
    let (oldx, oldy, _) = blockAt oldMap physicalPlayer.position
    let (bx, by, blockHit) = dummyColl physicalPlayer.position physicalPlayer.size velocity oldMap
    match blockHit with
        | None -> LevelState({ level = level; player = player; camera = camera; timePlayed = timePlayed })
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
                | AddItem item -> raise (NotImplementedException())
                | CollisionAction.NextLevel -> raise (NotImplementedException())

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
                                    | Some tile -> tile.standOnAction
            let gamePlayer = updatePlayerEffects levelState.player standingAction time
            if gamePlayer.health <= 0 then
                EndScreen({ player = gamePlayer; totalTimePlayed = levelState.timePlayed + time.ElapsedGameTime; endState = GameLost })
            else
                updateLevel levelState.level gamePlayer levelState.camera event standingAction (levelState.timePlayed + time.ElapsedGameTime)
        | EndScreen endScreenState -> EndScreen(endScreenState)
                    

let drawPlayer (graphics: GraphicsState) (player: LevelPlayer) =
    let (texture, region) = getTile graphics.tileset 2
    let scale = player.size / float32(region.Size.X)
    do graphics.batch.Draw(texture, player.position, System.Nullable(region), Color.Blue, 0.0f, emptyVec, scale, SpriteEffects.None, 0.0f)

let drawMap (graphics: GraphicsState) (map: JumpAndRun.Map) =
    mapIteri (fun x y block ->
        match block with
            | None -> ()
            | Some block ->
                let (texture, region) = getTile graphics.tileset block.tileType
                let (x, y) = block.coordinates
                graphics.batch.Draw(texture, Rectangle(x * tileSize, y * tileSize, tileSize, tileSize), System.Nullable(region), block.color)
    ) map                

let drawEnemies (graphics: GraphicsState) (enemies: Enemy seq) =
    let (tex, reg) = getTile graphics.tileset 9
    for enemy in enemies do
        let scale = enemy.size / float32(reg.Size.X)
        graphics.batch.Draw(tex, enemy.position, System.Nullable(reg), Color.Red, 0.0f, emptyVec, scale, SpriteEffects.None, 0.0f)

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
            drawMap graphics state.level.map
            drawPlayer graphics state.level.player
            drawEnemies graphics state.level.enemies
            batch.End()
            batch.Begin() //Draw gui
            let healthColor = match state.player.damage with
                                    | None -> Color.White
                                    | Some _ -> Color.Red
            batch.DrawString(graphics.font, (sprintf "Health %i" state.player.health), Vector2(0.0f, 50.0f), healthColor)
            batch.DrawString(graphics.font, (sprintf "Level %i" state.player.level), Vector2(0.0f, 0.0f), Color.White)
            batch.End()
        | EndScreen endScreenState ->
            ()
    ()