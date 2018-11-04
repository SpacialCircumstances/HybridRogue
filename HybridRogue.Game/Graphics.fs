module HybridRogue.Game.Graphics

open Microsoft.Xna.Framework.Graphics

type GraphicsState = { batch: SpriteBatch; font: SpriteFont }

let createGraphicsState (batch: SpriteBatch) (font: SpriteFont) =
    { batch = batch; font = font }