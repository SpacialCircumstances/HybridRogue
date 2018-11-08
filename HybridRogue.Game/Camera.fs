module HybridRogue.Game.Camera

open Microsoft.Xna.Framework

type Camera = { position: Vector2; scale: float32 }

let createCamera (position: Vector2) (scale: float32) =
    { position = position; scale = scale }

let calculateTransform (camera: Camera) (viewportSize: Point) =
    let translationMatrix = Matrix.CreateTranslation(-camera.position.X, -camera.position.Y, 0.0f)
    let scaleMatrix = Matrix.CreateScale(camera.scale, camera.scale, 1.0f)
    let viewportMatrix = Matrix.CreateTranslation(float32(viewportSize.X) * 0.5f, float32(viewportSize.Y) * 0.5f, 0.0f)
    translationMatrix * scaleMatrix * viewportMatrix