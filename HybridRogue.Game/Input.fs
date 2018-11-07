module HybridRogue.Game.Input

open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework
open System

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

let singleMouseEvent (old: ButtonState) (current: ButtonState) (button: MouseButton) (pos: Vector2) :InputEvent option =
    match old with
            | ButtonState.Pressed ->
                match current with
                        | ButtonState.Pressed -> None
                        | ButtonState.Released -> Some(Up(button, pos))
                        | _ -> raise (new ArgumentOutOfRangeException())
            | ButtonState.Released ->
                match current with
                        | ButtonState.Pressed -> Some(Down(button, pos))
                        | ButtonState.Released -> None
                        | _ -> raise (new ArgumentOutOfRangeException())
            | _ -> raise (new ArgumentOutOfRangeException())

let keyboardEvents (oldKeys: Keys array) (newKeys: Keys array) =
    if ((Array.isEmpty oldKeys) && (Array.isEmpty newKeys)) then
        None
    else
        let released = Seq.ofArray (Array.filter (fun k -> not (Array.contains k newKeys)) oldKeys)
        let pressed = Seq.ofArray (Array.filter (fun k -> not (Array.contains k oldKeys)) newKeys)
        let releasedEvents = Seq.map Released released
        let pressedEvents = Seq.map Pressed pressed
        let events = Seq.concat [releasedEvents; pressedEvents]
        if Seq.isEmpty events then
            None
        else
            Some(events)

let scrollEvents (oldValue: int) (newValue: int) =
    if oldValue = newValue then
        None
    else
        Some(Scrolled(newValue - oldValue))

let updateInput (oldInputState: InputState) (pressedKeys: Keys array) (mouse: MouseState) (text: TextEvent option) =
    let oldMouseState = oldInputState.lastMouseState
    let event = OperationCombinators.orElse {
        return! singleMouseEvent oldMouseState.LeftButton mouse.LeftButton Left (mouse.Position.ToVector2())
        return! singleMouseEvent oldMouseState.RightButton mouse.RightButton Right (mouse.Position.ToVector2())
        return! singleMouseEvent oldMouseState.MiddleButton mouse.MiddleButton Middle (mouse.Position.ToVector2())
        return! scrollEvents oldMouseState.ScrollWheelValue mouse.ScrollWheelValue
        return! match text with
                    | None -> None
                    | Some t -> Some(TextInput(t))
        return! match (keyboardEvents oldInputState.lastPressedKeys pressedKeys) with
                    | Some events -> Seq.tryHead events
                    | None -> None
    }
    let newInputState = { lastPressedKeys = pressedKeys; lastMouseState = mouse }
    (newInputState, event)