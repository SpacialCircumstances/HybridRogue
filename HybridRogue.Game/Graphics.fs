module HybridRogue.Game.Graphics

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework

type Tileset = { texture: Texture2D; tileSize: int }

type GraphicsState = { batch: SpriteBatch; font: SpriteFont; tileset: Tileset; viewportSize: Point }

let getTile (tileset: Tileset) (index: int): Texture2D * Rectangle =
    let tilesInRow = tileset.texture.Width / tileset.tileSize
    let tileX = tilesInRow % index
    let tileY = index / tilesInRow
    (tileset.texture, Rectangle(tileX * tileset.tileSize, tileY * tileset.tileSize, tileset.tileSize, tileset.tileSize))

let loadGraphics (batch: SpriteBatch) (content: ContentManager) (viewportSize: Point) =
    { batch = batch; font = content.Load<SpriteFont>("fonts/Square"); tileset = { texture = content.Load<Texture2D>("tilesets/Square"); tileSize = 64 }; viewportSize = viewportSize }