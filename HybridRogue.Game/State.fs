module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework

type LevelState = { level: int }

type GameState = 
        | MenuState
        | LevelState of LevelState

let initialGameState = LevelState({ level = 1 })

let updateState (state: GameState) =
    state

let drawState (state: GameState) (graphics: GraphicsState) =
    let batch = graphics.batch
    batch.Begin()
    match state with
        | MenuState ->
            ()
        | LevelState state ->
            batch.DrawString(graphics.font, (sprintf "Level %i" state.level), Vector2(0.0f, 0.0f), Color.White)
    batch.End()
    ()