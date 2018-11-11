module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Input
open Level
open Camera
open Microsoft.Xna.Framework.Graphics

let tileSize = 16

let gravity = Vector2(0.0f, 0.02f)

type Player = { name: string; level: int }

let emptyPlayer = { name = "TestDummy"; level = 1 }

type LevelState = { level: Level; player: Player; camera: Camera }

type CollisionAction = Move of JumpAndRun.LevelPlayer

type GameState = 
        | MenuState
        | LevelState of LevelState

let initialGameState = MenuState

let accFactor = 0.01f
let leftAcc = Vector2(-accFactor, 0.0f)
let rightAcc = Vector2(accFactor, 0.0f)
let upAcc = Vector2(0.0f, -accFactor)
let downAcc = Vector2(0.0f, accFactor)

let calculateAcceleration (event: InputEvent) =
    match event with
        | Pressed key ->
            match key with
                | Keys.Left ->
                    leftAcc
                | Keys.Right ->
                    rightAcc
                | Keys.Up ->
                    upAcc
                | _ -> Vector2(0.0f, 0.0f)
        | Released key ->
            match key with
                | Keys.Left ->
                    rightAcc
                | Keys.Right ->
                    leftAcc
                | Keys.Up ->
                    downAcc
                | _ -> Vector2(0.0f, 0.0f)
        | _ -> Vector2(0.0f, 0.0f)
   
let calculateNewPlayerPosition (player: JumpAndRun.LevelPlayer) (event: InputEvent option) (time: GameTime): JumpAndRun.LevelPlayer =
    let acc = match event with
                | Some event ->
                    calculateAcceleration event
                | None -> Vector2(0.0f, 0.0f)
    let newAcc = player.acceleration + acc
    let totalAcc = newAcc + gravity
    let timeFactor = float32(time.ElapsedGameTime.TotalSeconds * 100.0)
    let velocity = player.velocity + Vector2(totalAcc.X * timeFactor, totalAcc.Y * timeFactor)
    let newPosition = (player.target.Location + (Vector2(velocity.X * timeFactor, velocity.Y * timeFactor)).ToPoint())
    { target = Rectangle(newPosition, player.target.Size); velocity = velocity; acceleration = newAcc }

let collisionCheck (lastPlayer: JumpAndRun.LevelPlayer) (nextPlayer: JumpAndRun.LevelPlayer) (map: JumpAndRun.Map) =
    let clampToMapCoords = JumpAndRun.clampToMapCoords map (nextPlayer.target.Location + nextPlayer.target.Size)
    let (bx, by, blockTarget) = JumpAndRun.blockAt map clampToMapCoords
    match blockTarget with
        | None ->
            Move({ nextPlayer with target = Rectangle(clampToMapCoords - nextPlayer.target.Size, nextPlayer.target.Size) })
        | Some block -> 
            let blockBox = JumpAndRun.tileRect bx by
            Move({ nextPlayer with target = Rectangle(blockBox.Location - nextPlayer.target.Size, nextPlayer.target.Size) })
   
let updatePlayerAndCamera (map: JumpAndRun.Map) (player: JumpAndRun.LevelPlayer) (camera: Camera) (event: InputEvent option) (time: GameTime) =
    let newPlayer: JumpAndRun.LevelPlayer = calculateNewPlayerPosition player event time
    let collisionAction = collisionCheck player newPlayer map
    let playerAfterMove = match collisionAction with
                                | Move next -> next
    let newCamera = { scale = camera.scale; position = playerAfterMove.target.Location.ToVector2() }
    (playerAfterMove, newCamera)

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