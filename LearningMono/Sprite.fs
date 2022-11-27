module Sprite

open Microsoft.Xna.Framework
open Xelmish.Model
open Elmish
open Xelmish.Viewables
open Config

type Model =
    { Images: ImageConfig list

      CurrentImage: ImageConfig
      RelativeYPos: int

      FrameXPos: int
      AnimationRunning: bool

      Tint: Color
      LastFrameTime: int64
      FrameLength: int64
      FlipH: bool
      ScreenPos: Vector2 }

let currentImageConfigAndRelativePos images pos =
    let (_, y) = pos

    let rec getImage yPos imagesList =
        match imagesList with
        | [] -> failwith "At least one image per sprite pls programmer san"
        | lastImage :: [] -> lastImage, yPos
        | image :: rest ->
            let rem = image.Rows - 1 - yPos
            if rem >= 0 then image, yPos else getImage -rem rest

    getImage y images

let init pos (config: SpriteConfig) =
    let (img, yPos) = currentImageConfigAndRelativePos config.Images config.InitPos

    { Images = config.Images

      CurrentImage = img
      RelativeYPos = yPos

      FrameXPos = fst(config.InitPos)
      Tint = config.Tint
      AnimationRunning = false
      FlipH = false

      LastFrameTime = 0L
      FrameLength = 300L
      ScreenPos = pos }

type Message =
    | Stop
    | Animate of int * int64
    | AnimTick of int64
    | SetPos of Vector2
    | SetDir of bool


let spriteSourceRect (spriteInfo: ImageConfig) pos =
    let totalWidth, totalHeight = spriteInfo.PixelSize
    let xPos, yPos = pos

    let height = totalHeight / spriteInfo.Rows
    let width = totalWidth / spriteInfo.Columns
    let x, y = xPos * width, yPos * height
    rect x y width height


let drawSprite (model: Model) =
    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->

        let texture = loadedAssets.textures[model.CurrentImage.TextureName]
        let sourceRect = spriteSourceRect model.CurrentImage (model.FrameXPos, model.RelativeYPos)

        spriteBatch.Draw(
            texture,
            Rectangle(int (model.ScreenPos.X), int (model.ScreenPos.Y), sourceRect.Width, sourceRect.Height),
            sourceRect,
            model.Tint,
            0f,
            Vector2(float32 (sourceRect.Width / 2), float32 (sourceRect.Height / 2)),
            (if model.FlipH then
                 Graphics.SpriteEffects.FlipHorizontally
             else
                 Graphics.SpriteEffects.None),
            0f
        ))

let animTick model dt =
    let t = model.LastFrameTime + dt

    let (t, inc) =
        match t > model.FrameLength with
        | true -> (model.LastFrameTime - model.FrameLength, 1)
        | false -> model.LastFrameTime, 0

    let oldX = model.FrameXPos

    let newPos =
        if not model.AnimationRunning then
            model.FrameXPos
        else
            (oldX + inc) % model.CurrentImage.Columns

    { model with
        FrameXPos = newPos
        LastFrameTime = t }

let update message model =
    match message with
    | Stop ->
        { model with
            FrameXPos = model.FrameXPos
            AnimationRunning = false },
        Cmd.none
    | Animate(which, increment) ->
        let (img, yPos) = currentImageConfigAndRelativePos model.Images (0, which)
        { model with
            CurrentImage = img
            RelativeYPos = yPos
            FrameXPos = 0
            FrameLength = increment
            AnimationRunning = true },
        Cmd.none
    | AnimTick dt -> animTick model dt, Cmd.none
    | SetPos p ->
        { model with ScreenPos = p }, Cmd.none
    | SetDir flipH -> { model with FlipH = flipH }, Cmd.none


let view model (dispatch: Message -> unit) =
    [ yield drawSprite model
      yield onupdate (fun input -> dispatch (AnimTick input.totalGameTime)) ]
