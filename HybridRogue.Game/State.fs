﻿module HybridRogue.Game.State

open HybridRogue.Game.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Input

type LevelState = { level: int }

type GameState = 
        | MenuState
        | LevelState of LevelState

let initialGameState = MenuState

let updateState (state: GameState) (event: InputEvent option) =
    match state with
        | MenuState ->
            match event with
                | None -> state
                | Some event ->
                    match event with
                        | Released key ->
                            if key = Keys.Enter then
                                LevelState({ level = 1 })
                            else
                                state
                        | _ -> state
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