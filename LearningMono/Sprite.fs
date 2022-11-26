module Sprite

open Microsoft.Xna.Framework
open Xelmish.Model
open Elmish
open Xelmish.Viewables
open Config

type Model =
    { PixelSize: int * int
      Rows: int
      Columns: int
      Pos: int * int
      AnimationRunning: bool

      TextureName: string
      Tint: Color

      LastFrameTime: int64
      FrameLength: int64

      FlipH: bool

      ScreenPos: Vector2 }


let init pos (config: SpriteConfig) =
    { PixelSize = config.PixelSize
      Rows = config.Rows
      Columns = config.Columns
      Pos = config.InitPos
      TextureName = config.TextureName
      Tint = config.Tint
      AnimationRunning = false
      FlipH = false

      LastFrameTime = 0L
      FrameLength = 300L
      ScreenPos = pos }

type Message =
    | Stop of int
    | Animate of int * int64
    | AnimTick of int64
    | SetPos of Vector2
    | SetDir of bool

let spriteSourceRect (spriteInfo: Model) =
    let totalWidth, totalHeight = spriteInfo.PixelSize
    let xPos, yPos = spriteInfo.Pos

    let height = totalHeight / spriteInfo.Rows
    let width = totalWidth / spriteInfo.Columns
    let x, y = xPos * width, yPos * height
    rect x y width height


let drawSprite (model: Model) =
    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->
        let texture = loadedAssets.textures[model.TextureName]
        let sourceRect = spriteSourceRect model

        spriteBatch.Draw(
            texture,
            Rectangle(int (model.ScreenPos.X), int (model.ScreenPos.Y), sourceRect.Width, sourceRect.Height),
            sourceRect,
            model.Tint,
            0f,
            Vector2(float32(sourceRect.Width / 2), float32(sourceRect.Height / 2)),
            (if model.FlipH then Graphics.SpriteEffects.FlipHorizontally else Graphics.SpriteEffects.None),
            0f
        ))

let animTick model dt =
    let t = model.LastFrameTime + dt

    let (t, inc) =
        match t > model.FrameLength with
        | true -> (model.LastFrameTime - model.FrameLength, 1)
        | false -> model.LastFrameTime, 0

    let oldX, oldY = model.Pos
    let newPos =
        if not model.AnimationRunning then
            model.Pos
        else
            ((oldX + inc) % model.Columns, oldY)

    { model with
        Pos = newPos
        LastFrameTime = t }

let update message model =
    match message with
    | Stop y -> { model with Pos = (0, y); AnimationRunning = false }, Cmd.none
    | Animate(which, increment) ->
        { model with
            Pos = (0, which)
            FrameLength = increment
            AnimationRunning = true },
        Cmd.none
    | AnimTick dt -> animTick model dt, Cmd.none
    | SetPos p -> { model with ScreenPos = p }, Cmd.none
    | SetDir flipH -> { model with FlipH = flipH }, Cmd.none


let view model (dispatch: Message -> unit) =
    [ yield drawSprite model
      yield onupdate (fun input -> dispatch (AnimTick input.totalGameTime)) ]
