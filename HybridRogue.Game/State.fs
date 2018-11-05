module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework

type GameState = { level: int }

let initialGameState = { level = 1 }

let updateState (state: GameState) =
    state

let drawState (state: GameState) (graphics: GraphicsState) =
    let batch = graphics.batch
    batch.Begin()
    batch.DrawString(graphics.font, (sprintf "Level %i" state.level), Vector2(0.0f, 0.0f), Color.White)
    batch.End()
    ()