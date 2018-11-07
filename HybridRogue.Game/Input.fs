module HybridRogue.Game.Input

open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework

type MouseButton = Left | Middle | Right

type TextEvent = { character: char option; key: Keys }

type InputEvent = 
        | Down of  MouseButton * Vector2
        | Up of MouseButton * Vector2
        | Scrolled of int
        | Pressed of Keys
        | Released of Keys
        | TextInput of TextEvent

type InputState = { lastPressedKeys: Keys array; lastMouseState: MouseState }

let emptyInputState = { lastPressedKeys = [||]; lastMouseState = MouseState() }

let updateInput (oldInputState: InputState) (pressedKeys: Keys array) (mouse: MouseState) (texts: TextEvent seq) =
    (emptyInputState, None) //TODO