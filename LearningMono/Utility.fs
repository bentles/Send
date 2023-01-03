module Utility

open Microsoft.Xna.Framework
open Config
open Collision

let coordsToPos (xx: float32) (yy: float32) (half: Vector2) =
    let startX = 0f
    let startY = 0f

    let xBlockOffSet = float32 (xx * float32 worldConfig.TileWidth)
    let yBlockOffSet = float32 (yy * float32 worldConfig.TileWidth)

    let actualX = startX + xBlockOffSet + half.X
    let actualY = startY + yBlockOffSet + half.Y

    Vector2(actualX, actualY)

let posToCoords (pos: Vector2) : (int * int) =
    let x = int (pos.X / float32 worldConfig.TileWidth)
    let y = int (pos.Y / float32 worldConfig.TileWidth)
    (x, y)

let posRounded (pos: Vector2) (worldConfig: WorldConfig) =
    let x =
        (floor (pos.X / float32 worldConfig.TileWidth)) * float32 worldConfig.TileWidth

    let y =
        (floor (pos.Y / float32 worldConfig.TileWidth)) * float32 worldConfig.TileWidth

    Vector2(x, y) + Vector2(float32 (worldConfig.TileWidth / 2))

let createColliderFromCoords (xx: float32) (yy: float32) (half: Vector2) =
    { Pos = coordsToPos xx yy half
      Half = half }

