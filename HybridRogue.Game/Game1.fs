namespace HybridRogue.Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open State
open Graphics
open Input
open System

type Game1() as this = 
    inherit Microsoft.Xna.Framework.Game()
    do this.Content.RootDirectory <- "Content"
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let graphics = new GraphicsDeviceManager(this)
    let mutable state = Unchecked.defaultof<GameState>
    let mutable graphicsState = Unchecked.defaultof<GraphicsState>
    let mutable lastInputState = emptyInputState
    let mutable textInputEvent = None
    do this.IsMouseVisible <- true
    do this.Window.TextInput.AddHandler(EventHandler<TextInputEventArgs>(fun sender args -> 
        do textInputEvent <- Some({ character = Some(args.Character); key = args.Key })
    ))

    override this.Initialize() =
        base.Initialize()
        do this.Window.Title <- "Hybrid Rogue"
        do graphics.PreferredBackBufferHeight <- 600
        do graphics.PreferredBackBufferWidth <- 800
        do graphics.ApplyChanges ()
        do state <- initialGameState

    override this.LoadContent() =
        base.LoadContent()
        do spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
        do graphicsState <- loadGraphics spriteBatch this.Content (Point(800, 600))
    
    override this.Update(time: GameTime) =
        let keyboardState = Keyboard.GetState()
        let (inputState, event) = updateInput lastInputState (keyboardState.GetPressedKeys()) (Mouse.GetState()) textInputEvent
        do lastInputState <- inputState
        do textInputEvent <- None
        do state <- updateState state event time
    
    override this.Draw(time: GameTime) =
        graphics.GraphicsDevice.Clear Color.Black
        drawState state graphicsState