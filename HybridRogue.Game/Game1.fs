namespace HybridRogue.Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open State

type Game1() as this = 
    inherit Microsoft.Xna.Framework.Game()
    do this.Content.RootDirectory <- "Content"
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let graphics = new GraphicsDeviceManager(this)
    let mutable state = Unchecked.defaultof<GameState>

    override this.Initialize() =
        do this.Window.Title <- "Hybrid Rogue"
        do graphics.PreferredBackBufferHeight <- 600
        do graphics.PreferredBackBufferWidth <- 800
        do graphics.ApplyChanges ()
        do state <- initialGameState

    override this.LoadContent() =
        do spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
    
    override this.Update(time: GameTime) =
        do state <- updateState state
    
    override this.Draw(time: GameTime) =
        graphics.GraphicsDevice.Clear Color.CornflowerBlue