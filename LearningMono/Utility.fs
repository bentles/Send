module Utility

open Microsoft.Xna.Framework
open Xelmish.Model
open Xelmish.Viewables
open Prelude
open GameConfig

let CoordsFToOffsetVector ((x,y): CoordsF) (half: Vector2) =
    let startX = 0f
    let startY = 0f

    let xBlockOffSet = float32 (x * float32 worldConfig.TileWidth)
    let yBlockOffSet = float32 (y * float32 worldConfig.TileWidth)

    let actualX = startX + xBlockOffSet + half.X
    let actualY = startY + yBlockOffSet + half.Y

    Vector2(actualX, actualY)


let offsetVectorToCoords (pos: Vector2) : Coords =
    let x = int (pos.X / float32 worldConfig.TileWidth)
    let y = int (pos.Y / float32 worldConfig.TileWidth)
    (x, y)

let toCoordsF ((x, y): Coords) : CoordsF = (float32 x, float32 y)

let posRounded (pos: Vector2) =
    let x =
        (floor (pos.X / float32 worldConfig.TileWidth)) * float32 worldConfig.TileWidth

    let y =
        (floor (pos.Y / float32 worldConfig.TileWidth)) * float32 worldConfig.TileWidth

    Vector2(x, y) + Vector2(float32 (worldConfig.TileWidth / 2))

let toIndex ((x, y): Coords) (width: int) = y * width + x

let posToIndex (pos: Vector2) (width: int) : int =
    let coords = offsetVectorToCoords pos
    toIndex coords width

let coordsOutOfBounds ((x, y): Coords) (width: int, height: int) : bool =
    x >= width || x < 0 || y >= height || y < 0

let coordsToIndex (coords: Coords) ((width: int, height: int): Coords) : int voption =
    if coordsOutOfBounds coords (width, height) then
        ValueNone
    else
        ValueSome(toIndex coords width)

let createColliderFromCoords (coordsF: CoordsF) (half: Vector2) =
    { Pos = CoordsFToOffsetVector coordsF half
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

let round (x: float32) = int (System.Math.Round(float x))

let debugText s (x, y) =
    text "defaultFont" 20. Colour.White (0, 0) s (x, y)

let directions up down left right event =
    onupdate (fun inputs ->
        let y = if inputs.keyboardState.IsKeyDown up then -1 else 0
        let y = y + if inputs.keyboardState.IsKeyDown down then 1 else 0
        let x = if inputs.keyboardState.IsKeyDown left then -1 else 0
        let x = x + if inputs.keyboardState.IsKeyDown right then 1 else 0
        event (Vector2(float32 x, float32 y)))

let rotateFacing (facing: Facing) (clock: bool) =
    match facing, clock with
    | FacingLeft, true
    | FacingRight, false -> FacingUp
    | FacingUp, true
    | FacingDown, false -> FacingRight
    | FacingRight, true
    | FacingLeft, false -> FacingDown
    | FacingDown, true
    | FacingUp, false -> FacingLeft

let facingToCoords (facing: Facing) : Coords =
    match facing with
    | FacingLeft -> (-1, 0)
    | FacingRight -> (1, 0)
    | FacingUp -> (0, -1)
    | FacingDown -> (0, 1)

let vectorToFacing (vec: Vector2) : Facing voption =
    match vec.X, vec.Y with
    | 1f, 0f -> ValueSome FacingRight
    | -1f, 0f -> ValueSome FacingLeft
    | 0f, -1f -> ValueSome FacingUp
    | 0f, 1f -> ValueSome FacingDown
    | _ -> ValueNone
