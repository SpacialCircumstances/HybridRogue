namespace HybridRogue.Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open State
open Graphics

type Game1() as this = 
    inherit Microsoft.Xna.Framework.Game()
    do this.Content.RootDirectory <- "Content"
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let graphics = new GraphicsDeviceManager(this)
    let mutable state = Unchecked.defaultof<GameState>
    let mutable graphicsState = Unchecked.defaultof<GraphicsState>
    do this.IsMouseVisible <- true

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
        let font = this.Content.Load<SpriteFont>("fonts/Square")
        do graphicsState <- createGraphicsState spriteBatch font
    
    override this.Update(time: GameTime) =
        do state <- updateState state
    
    override this.Draw(time: GameTime) =
        graphics.GraphicsDevice.Clear Color.Black
        drawState state graphicsState