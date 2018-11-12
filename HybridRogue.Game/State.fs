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

let tileSize = 16

let gravity = Vector2(0.0f, 0.08f)

type Player = { name: string; level: int }

let emptyPlayer = { name = "TestDummy"; level = 1 }

type LevelState = { level: Level; player: Player; camera: Camera }

type CollisionAction = Move of Point * Vector2

type GameState = 
        | MenuState
        | LevelState of LevelState

let initialGameState = MenuState

let emptyVec = Vector2(0.0f, 0.0f)

let normalVel = 1.0f
let maxVelC = 10.0f

let clamp minimum maximum value =
    max (min maximum value) minimum

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
  
let calculateNewPlayerPosition (player: JumpAndRun.LevelPlayer) (event: InputEvent option) (time: GameTime): Point * Vector2 =
    let vel = match event with
                        | Some event ->
                            calculateVelocity player.velocity event
                        | None -> player.velocity
    let timeFactor = float32(time.ElapsedGameTime.TotalSeconds * 100.0)
    let velocity = gravity + vel
    let velocity = Vector2(clamp -maxVelC maxVelC velocity.X, clamp -maxVelC maxVelC velocity.Y)
    let newPosition = (player.target.Location + (Vector2(velocity.X * timeFactor, velocity.Y * timeFactor)).ToPoint())
    (newPosition, velocity)

let collisionCheck (target: Rectangle) (velocity: Vector2) (map: JumpAndRun.Map) =
    let position = target.Location
    let size = target.Size
    let hsize = Point(size.X / 2, size.Y / 2)
    let distance = int(velocity.Length())
    let frac = 1.0f / (float32(distance + 1))
    let isBlock (pos: Point) =
        let (_, _, b) = JumpAndRun.blockAt map pos
        match b with
            | None -> false
            | Some _ -> true
    
    let blockHit (pos: Point) =
        isBlock (Point(pos.X - hsize.X, pos.Y - hsize.Y)) ||
        isBlock (Point(pos.X - hsize.X, pos.Y + hsize.Y)) ||
        isBlock (Point(pos.X + hsize.X, pos.Y - hsize.Y)) ||
        isBlock (Point(pos.X + hsize.X, pos.Y + hsize.Y))

    let blockAt = JumpAndRun.blockAt map
    let (finalVel, finalPos) = List.fold (fun (vel: Vector2, pos: Point) i ->
                                    let step = (Vector2(vel.X * frac, vel.Y * frac)).ToPoint()
                                    let newPos = pos + step
                                    let block = blockHit newPos
                                    match block with
                                        | false ->
                                            (vel, pos)
                                        | true ->
                                            let yHit = blockHit (Point(pos.X, newPos.Y))
                                            match yHit with
                                                | true ->
                                                    (Vector2(vel.X, 0.0f), Point(newPos.X, pos.Y))
                                                | false ->
                                                    let xHit = blockHit (Point(newPos.X, pos.Y))
                                                    match xHit with
                                                        | true ->
                                                            (Vector2(0.0f, vel.Y), Point(pos.X, newPos.Y))
                                                        | false ->
                                                            (emptyVec, pos)
                                    ) (velocity, position) [0..distance]
    Move(finalPos, finalVel)
   
let updatePlayerAndCamera (map: JumpAndRun.Map) (player: JumpAndRun.LevelPlayer) (camera: Camera) (event: InputEvent option) (time: GameTime) =
    let (pos, vel) = calculateNewPlayerPosition player event time
    let collisionAction = collisionCheck (Rectangle(pos, player.target.Size)) vel map
    match collisionAction with
            | Move (pos, vel) ->
                let newCamera = { scale = camera.scale; position = pos.ToVector2() }
                let newPlayer: JumpAndRun.LevelPlayer = { target = Rectangle(pos, player.target.Size); velocity = vel }
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
    do graphics.batch.Draw(texture, player.target, System.Nullable(region), Color.Blue)

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