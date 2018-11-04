module HybridRogue.Game.State

open HybridRogue.Game.Graphics

type GameState = { level: int }

let initialGameState = { level = 1 }

let updateState (state: GameState) =
    state

let drawState (state: GameState) (graphics: GraphicsState) =
    ()