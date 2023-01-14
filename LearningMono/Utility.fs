﻿module Utility

open Microsoft.Xna.Framework
open Config
open Collision
open Xelmish.Model

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

let imageWithSource (key: string) (colour: Color) (srcSize: int * int) (srcPos: int * int) (destSize: int * int) (destPos: int * int): Viewable =
   
    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->
        let texture = loadedAssets.textures[key]
        let (x, y) = destPos
        let (width, height) = destSize

        let (sourceX, sourceY) = srcPos
        let (sourceWidth, sourceHeight) = srcSize

        spriteBatch.Draw(
            texture,
            Rectangle(x, y, width, height),
            Rectangle(sourceX, sourceY, sourceWidth, sourceHeight),
            colour,
            0f,
            Vector2.Zero,            
            Graphics.SpriteEffects.None,
            0f
        ))
    