module Utility

open Microsoft.Xna.Framework
open Config
open Collision
open Xelmish.Model
open Xelmish.Viewables

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

let posToIndex (pos: Vector2) : int =
    let x, y = posToCoords pos
    y * worldConfig.WorldTileLength + x

let coordsOutOfBounds (x: int, y: int) : bool =
    x >= worldConfig.WorldTileLength
    || x < 0
    || y >= worldConfig.WorldTileLength
    || y < 0

let coordsToIndex (x: int, y: int) : int option =
    if coordsOutOfBounds (x, y) then
        None
    else
        Some(y * worldConfig.WorldTileLength + x)

let createColliderFromCoords (xx: float32) (yy: float32) (half: Vector2) =
    { Pos = coordsToPos xx yy half
      Half = half }

let imageWithSource
    (key: string)
    (colour: Color)
    (srcSize: int * int)
    (srcPos: int * int)
    (destSize: int * int)
    (destPos: int * int)
    : Viewable =

    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->
        let (x, y) = destPos
        let (width, height) = destSize
        let (sourceX, sourceY) = srcPos
        let (sourceWidth, sourceHeight) = srcSize

        spriteBatch.Draw(
            loadedAssets.textures[key],
            Rectangle(x, y, width, height),
            Rectangle(sourceX, sourceY, sourceWidth, sourceHeight),
            colour,
            0f,
            Vector2.Zero,
            Graphics.SpriteEffects.None,
            0f
        ))

let round (x:float32) = int (System.Math.Round(float x))

let debugText s (x, y) =
    text "defaultFont" 20. Colour.White (0, 0) s (x , y)

let directions up down left right event =
    onupdate (fun inputs ->
        let y = if inputs.keyboardState.IsKeyDown up then -1 else 0
        let y = y + if inputs.keyboardState.IsKeyDown down then 1 else 0
        let x = if inputs.keyboardState.IsKeyDown left then -1 else 0
        let x = x + if inputs.keyboardState.IsKeyDown right then 1 else 0
        event (Vector2(float32 x, float32 y)))