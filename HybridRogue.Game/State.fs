module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Input
open Level
open Camera
open Microsoft.Xna.Framework.Graphics
open HybridRogue.Game
open HybridRogue.Game
open HybridRogue.Game
open System.Diagnostics
open HybridRogue.Game
open System

let tileSize = 16

let gravity = Vector2(0.0f, 0.08f)

type Player = { name: string; level: int }

let emptyPlayer = { name = "TestDummy"; level = 1 }

type LevelState = { level: Level; player: Player; camera: Camera }

type CollisionAction = Move of Vector2 * Vector2

type GameState = 
        | MenuState
        | LevelState of LevelState

let initialGameState = MenuState

let emptyVec = Vector2(0.0f, 0.0f)

let normalVel = 1.0f
let maxVelC = 10.0f

let clamp minimum maximum value =
    max (min maximum value) minimum

let clampVelocity = clamp -maxVelC maxVelC

let calculateVelocity (oldVel: Vector2) (event: InputEvent) =
    match event with
        | Pressed key ->
            match key with
                | Keys.Up -> Vector2(0.0f, -8.0f) + oldVel
                | Keys.Left -> Vector2(-normalVel, oldVel.Y)
                | Keys.Right -> Vector2(normalVel, oldVel.Y)
                | _ -> oldVel
        | Released key ->
            match key with
                | Keys.Left -> Vector2(0.0f, oldVel.Y)
                | Keys.Right -> Vector2(0.0f, oldVel.Y)
                | _ -> oldVel
        | _ -> oldVel

let collisionCheck (position: Vector2) (size: Vector2) (velocity: Vector2) (map: JumpAndRun.Map) =
    let blockAt = JumpAndRun.blockAt map
    let (x1, y1, _) = blockAt (position + size / 2.0f)
    let newPos = position + velocity
    let (x2, y2, newBlock) = 
        let (x, y, b) = blockAt (newPos)
        match b with
            | Some _ -> (x, y, b)
            | None ->
                let (x, y, b) = blockAt (newPos + size)
                match b with
                    | Some _ -> (x, y, b)
                    | None ->
                        let (x, y, b) = blockAt (Vector2(newPos.X, newPos.Y + size.Y))
                        match b with
                            | Some _ -> (x, y, b)
                            | None ->
                                blockAt (Vector2(newPos.X + size.X, newPos.Y))
                                
    let (finalVel, finalPos) = match newBlock with
                                    | None -> (velocity, newPos)
                                    | Some block ->
                                        let xd = abs(x2 - x1)
                                        let yd = abs(y2 - y1)
                                        if xd > 1 || yd > 1 then raise (InvalidOperationException())
                                        if xd = 1 then
                                            if yd = 0 then
                                                (Vector2(0.0f, velocity.Y), position)
                                            else
                                                (emptyVec, position)
                                        else
                                            (Vector2(velocity.X, 0.0f), position)

    Move(finalPos, finalVel)
   
let updatePlayerAndCamera (map: JumpAndRun.Map) (player: JumpAndRun.LevelPlayer) (camera: Camera) (event: InputEvent option) (time: GameTime) =
    let playerVelocity = match event with
                            | Some event ->
                                calculateVelocity player.velocity event
                            | None -> player.velocity
    let unclampedVel = playerVelocity + gravity
    let vel = Vector2(clampVelocity unclampedVel.X, clampVelocity unclampedVel.Y)
    let collisionAction = collisionCheck player.position player.size vel map
    match collisionAction with
            | Move (pos, vel) ->
                let newCamera = { scale = camera.scale; position = pos }
                let newPlayer: JumpAndRun.LevelPlayer = { position = pos; size = player.size; velocity = vel }
                (newPlayer, newCamera)

let updateState (state: GameState) (event: InputEvent option) (time: GameTime) =
    match state with
        | MenuState ->
            match event with
                | None -> state
                | Some event ->
                    match event with
                        | Released key ->
                            if key = Keys.Enter then
                                let level = defaultLevel
                                LevelState({ level =  level; player = emptyPlayer; camera = createCamera (Vector2(0.0f, 300.0f)) 1.0f })
                            else
                                state
                        | _ -> state
        | LevelState levelState ->
            let (newPlayer, newCamera) = updatePlayerAndCamera levelState.level.map levelState.level.player levelState.camera event time
            let newLevel: JumpAndRun.Level = { map = levelState.level.map; player = newPlayer }
            LevelState({ player = levelState.player; camera = newCamera; level = newLevel })
                    

let drawPlayer (graphics: GraphicsState) (player: JumpAndRun.LevelPlayer) =
    let (texture, region) = getTile graphics.tileset 2
    let playerRect = Rectangle(player.position.ToPoint(), player.size.ToPoint()) //TODO
    do graphics.batch.Draw(texture, playerRect, System.Nullable(region), Color.Blue)

let drawMap (graphics: GraphicsState) (map: JumpAndRun.Map) =
    JumpAndRun.mapIteri (fun x y block ->
        match block with
            | None -> ()
            | Some block ->
                let (texture, region) = getTile graphics.tileset block.tileType
                let (x, y) = block.coordinates
                graphics.batch.Draw(texture, Rectangle(x * JumpAndRun.tileSize, y * JumpAndRun.tileSize, JumpAndRun.tileSize, JumpAndRun.tileSize), System.Nullable(region), Color.White)
    ) map                

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
            batch.End()
            batch.Begin() //Draw gui
            batch.DrawString(graphics.font, (sprintf "Level %i" state.player.level), Vector2(0.0f, 0.0f), Color.White)
            batch.End()
    ()