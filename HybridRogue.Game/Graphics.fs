module HybridRogue.Game.Graphics

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content

type GraphicsState = { batch: SpriteBatch; font: SpriteFont; tileset: Texture2D }

let loadGraphics (batch: SpriteBatch) (content: ContentManager) =
    { batch = batch; font = content.Load<SpriteFont>("fonts/Square"); tileset = content.Load<Texture2D>("tilesets/Square") }