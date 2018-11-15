module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Input
open JumpAndRun
open Camera
open Microsoft.Xna.Framework.Graphics
open HybridRogue.Game
open System

let gravity = Vector2(0.0f, 0.12f)

type Player = { name: string; level: int }

let emptyPlayer = { name = "TestDummy"; level = 1 }

type LevelState = { level: Level; player: Player; camera: Camera }

type PostCollisionAction = 
    | Move of Vector2 * Vector2
    | NextLevel


type GameState = 
        | MenuState
        | LevelState of LevelState

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

let calculateVelocity (oldVel: Vector2) (event: InputEvent) =
    match event with
        | Pressed key ->
            match key with
                | Keys.Up -> Vector2(0.0f, -7.0f) + oldVel
                | _ -> velocityByPressedKeys oldVel (Keyboard.GetState())
        | _ -> velocityByPressedKeys oldVel (Keyboard.GetState())

let collisionCheck (position: Vector2) (size: Vector2) (velocity: Vector2) (map: JumpAndRun.Map) =
    let blockAt = JumpAndRun.blockAt map
    let (x1, y1, _) = blockAt position
    let newPos = position + velocity
    let (x2, y2, newBlock) = 
        let (x, y, b) = blockAt (newPos)
        match b with
            | Some _ -> (x, y, b)
            | None ->
                let (x, y, b) = blockAt (Vector2(newPos.X + size.X, newPos.Y))
                match b with
                    | Some _ -> (x - 1, y, b)
                    | None ->
                        let (x, y, b) = blockAt (Vector2(newPos.X, newPos.Y + size.Y))
                        match b with
                            | Some _ -> (x, y - 1, b)
                            | None ->
                                let (x, y, b) = blockAt (newPos + size)
                                (x - 1, y - 1, b)
                               
    match newBlock with
        | None -> Move(newPos, velocity)
        | Some block ->
            match block.collisionAction with
                | Stop ->
                    let xd = abs(x2 - x1)
                    let yd = abs(y2 - y1)
                    let (finalVel, finalPos) = if xd >= 1 then
                                                    if yd = 0 then
                                                        (Vector2(0.0f, velocity.Y), Vector2(position.X, newPos.Y))
                                                    else
                                                        (emptyVec, position)
                                                else
                                                    (Vector2(velocity.X, 0.0f), Vector2(newPos.X, position.Y))
                    Move(finalPos, finalVel)
                | CollisionAction.NextLevel -> NextLevel

   
let isOnFloor map pos =
    let (x, y, b) = blockAt map pos
    match getBlock map x (y + 1) with
        | None -> false
        | Some _ -> true

let updateLevelState (levelState: LevelState) (event: InputEvent option) (time: GameTime) =
    let player = levelState.level.player
    let map = levelState.level.map
    let camera = levelState.camera
    let playerVelocity = match event with
                            | Some event ->
                                calculateVelocity player.velocity event
                            | None -> player.velocity
    let onFloor = isOnFloor map (player.position + (player.size / 2.0f))
    let unclampedVel = if onFloor then playerVelocity else playerVelocity + gravity
    let vel = Vector2(clampVelocity unclampedVel.X, clampVelocity unclampedVel.Y)
    let collisionAction = collisionCheck player.position player.size vel map
    match collisionAction with
            | Move (pos, vel) ->
                let newCamera = { scale = camera.scale; position = pos }
                let newPlayer: LevelPlayer = { position = pos; size = player.size; velocity = vel }
                let newLevel = { player = newPlayer; map = map }
                { camera = newCamera; player = levelState.player; level = newLevel }
            | NextLevel ->
                printfn "Reached end of level"
                let newPlayer = { levelState.player with level = levelState.player.level + 1 }
                let newLevel = defaultLevel
                { camera = defaultCamera; player = newPlayer; level = newLevel }

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
                                LevelState({ level =  level; player = emptyPlayer; camera = defaultCamera })
                            else
                                state
                        | _ -> state
        | LevelState levelState ->
            let newState = updateLevelState levelState event time
            LevelState(newState)
                    

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