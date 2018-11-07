module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type LevelState = { level: int }

type GameState = 
        | MenuState
        | LevelState of LevelState

let initialGameState = MenuState

let updateState (state: GameState) =
    match state with
        | MenuState ->
            match Keyboard.GetState().IsKeyDown(Keys.Enter) with
                | true ->
                    LevelState({ level = 1})
                | false ->
                    state
        | LevelState levelState ->
            state

let drawState (state: GameState) (graphics: GraphicsState) =
    let batch = graphics.batch
    batch.Begin()
    match state with
        | MenuState ->
            batch.DrawString(graphics.font, "Press Enter to start a new game", Vector2(0.0f, 300.0f), Color.White)
        | LevelState state ->
            batch.DrawString(graphics.font, (sprintf "Level %i" state.level), Vector2(0.0f, 0.0f), Color.White)
    batch.End()
    ()