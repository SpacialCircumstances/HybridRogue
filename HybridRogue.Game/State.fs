module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Input
open Level
open Camera

type Player = { name: string; level: int }

let emptyPlayer = { name = "TestDummy"; level = 1 }

type LevelState = { level: Level; player: Player; camera: Camera }

type GameState = 
        | MenuState
        | LevelState of LevelState

let initialGameState = MenuState

let updateState (state: GameState) (event: InputEvent option) (time: GameTime) =
    match state with
        | MenuState ->
            match event with
                | None -> state
                | Some event ->
                    match event with
                        | Released key ->
                            if key = Keys.Enter then
                                LevelState({ level =  defaultLevel; player = emptyPlayer; camera = createCamera (Vector2(0.0f, 300.0f)) 1.0f })
                            else
                                state
                        | _ -> state
        | LevelState levelState ->
            state

let drawMap (graphics: GraphicsState) (map: JumpAndRun.Map) (player: Player) =
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
            drawMap graphics state.level.map state.player
            batch.End()
            batch.Begin() //Draw gui
            batch.DrawString(graphics.font, (sprintf "Level %i" state.player.level), Vector2(0.0f, 0.0f), Color.White)
            batch.End()
    ()