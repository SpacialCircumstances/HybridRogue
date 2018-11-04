module HybridRogue.Game.State

type GameState = { level: int }

let initialGameState = { level = 1 }

let updateState (state: GameState) =
    state