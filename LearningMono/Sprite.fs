module Sprite

open Microsoft.Xna.Framework
open Xelmish.Model
open Elmish
open Xelmish.Viewables
open Config

type AnimationState = Stopped | Looping | OnceOff

type Model =
    { Images: ImageConfig list

      CurrentImage: ImageConfig
      RelativeYPos: int

      FrameXPos: int
      AnimationState: AnimationState

      Tint: Color
      LastFrameTime: int64
      FrameLength: int64
      FlipH: bool
      ScreenPos: Vector2 }

let currentImageConfigAndRelativePos images pos =
    let (_, y) = pos

    // check the index is reasonable
    assert (y < (images |> List.sumBy (fun a -> a.Rows)))

    let rec getImage yPos imagesList =
        match imagesList with
        | [] -> failwith "At least one image per sprite pls programmer sama"
        | lastImage :: [] -> lastImage, yPos
        | image :: rest ->
            let rem = image.Rows - yPos
            if rem > 0 then image, yPos else getImage -rem rest

    getImage y images

let init pos (config: SpriteConfig) =
    let (img, yPos) = currentImageConfigAndRelativePos config.Images config.InitPos

    { Images = config.Images

      CurrentImage = img
      RelativeYPos = yPos

      FrameXPos = fst(config.InitPos)
      Tint = config.Tint
      AnimationState = Stopped
      FlipH = false

      LastFrameTime = 0L
      FrameLength = 300L
      ScreenPos = pos }

type Message =
    | Stop
    | SwitchAnimation of int * int64 
    | StartLoopingAnimation
    | StartOnceOffAnimation
    | AnimTick of int64
    | SetPos of Vector2
    | SetDirection of bool

type Events = 
    | None
    | AnimationComplete of int

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
        let spriteCenter = Vector2(float32 (sourceRect.Width / 2), float32 (sourceRect.Height / 2))
        spriteBatch.Draw(
            texture,
            Rectangle(int (model.ScreenPos.X), int (model.ScreenPos.Y), sourceRect.Width, sourceRect.Height),
            sourceRect,
            model.Tint,
            0f,
            Vector2.Add(spriteCenter, model.CurrentImage.Offset),
            (if model.FlipH then
                 Graphics.SpriteEffects.FlipHorizontally
             else
                 Graphics.SpriteEffects.None),
            0f
        ))

let animTick model dt =
    let t = model.LastFrameTime + dt

    let (t, inc) =
        match (t > model.FrameLength) with
        | true -> (model.LastFrameTime - model.FrameLength, 1)
        | false -> model.LastFrameTime, 0

    let oldX = model.FrameXPos
    let nextX = oldX + inc

    let (newPos, event) =
        if model.AnimationState = Stopped then
            model.FrameXPos, Events.None
        else if model.AnimationState = Looping then
            ((oldX + inc) % model.CurrentImage.Columns), Events.None
        else            
            let lastFrame = model.CurrentImage.Columns - 1
            if nextX > oldX && nextX = lastFrame then
                lastFrame, Events.AnimationComplete 1 //TODO: actual animation number
            else if nextX > lastFrame then
                lastFrame, Events.None
            else
                nextX, Events.None            

    { model with
        FrameXPos = newPos
        LastFrameTime = t }, event


let update message model =
    match message with
    | Stop ->
        { model with
            FrameXPos = model.FrameXPos
            AnimationState = Stopped },
        Events.None
    | SwitchAnimation(which, increment) ->
        let (img, yPos) = currentImageConfigAndRelativePos model.Images (0, which)
        { model with
            AnimationState = Stopped
            CurrentImage = img
            RelativeYPos = yPos
            FrameXPos = 0
            FrameLength = increment },
        Events.None
    | StartLoopingAnimation -> { model with AnimationState = Looping }, Events.None
    | StartOnceOffAnimation -> { model with AnimationState = OnceOff }, Events.None
    | AnimTick dt -> animTick model dt
    | SetPos p ->
        { model with ScreenPos = p }, Events.None
    | SetDirection flipH -> { model with FlipH = flipH }, Events.None


let view model (dispatch: Message -> unit) =
    [ yield drawSprite model
      yield onupdate (fun input -> dispatch (AnimTick input.totalGameTime)) ]
