namespace HybridRogue.Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Game1() as this = 
    inherit Microsoft.Xna.Framework.Game()
    do this.Content.RootDirectory <- "Content"
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let graphics = new GraphicsDeviceManager(this)

    override this.Initialize() =
        do this.Window.Title <- "Hybrid Rogue"
        do graphics.PreferredBackBufferHeight <- 600
        do graphics.PreferredBackBufferWidth <- 800
        do graphics.ApplyChanges ()

    override this.LoadContent() =
        do spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
    
    override this.Update(time: GameTime) =
        ()
    
    override this.Draw(time: GameTime) =
        graphics.GraphicsDevice.Clear Color.CornflowerBlue