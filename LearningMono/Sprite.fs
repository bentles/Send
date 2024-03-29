﻿[<RequireQualifiedAccess>]
module Sprite

open Microsoft.Xna.Framework
open Xelmish.Model
open GameConfig

[<Struct>]
type AnimationState =
    | Stopped of stop: struct (AnimationConfig * int)
    | Started of start: struct (AnimationConfig * int)

[<Struct>]
type Model =
    { Images: ImageConfig list

      CurrentImage: ImageConfig
      RelativeYPos: int

      AnimationState: AnimationState

      Tint: Color
      LastFrameTime: int64
      FrameLength: int64
      FlipH: bool
      FlipV: bool
      ScreenPos: Vector2 }

let currentImageConfigAndRelativePos images (animation: AnimationConfig) (ypos: int option) =
    let y = ypos |> Option.defaultValue animation.Index

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

let init pos time (config: SpriteConfig) (ypos: int option) (flipH: bool option) =
    match config with
    | SingleSpriteConfig singleConfig ->
        { Images = [ singleConfig.Image ]
          CurrentImage = singleConfig.Image
          RelativeYPos = ypos |> Option.defaultValue 0
          Tint = singleConfig.Tint
          AnimationState = Stopped(imageSpriteConfig, 0)
          FlipH = flipH |> Option.defaultValue false
          FlipV = false
          LastFrameTime = time
          FrameLength = singleConfig.FrameLength
          ScreenPos = pos }

    | AnimatedSpriteConfig aniConfig ->
        let (img, yPos) =
            currentImageConfigAndRelativePos aniConfig.Images aniConfig.InitAnimation ypos

        { Images = aniConfig.Images
          CurrentImage = img
          RelativeYPos = yPos
          Tint = aniConfig.Tint
          AnimationState =
            if aniConfig.Started then
                Started(aniConfig.InitAnimation, 0)
            else
                Stopped(aniConfig.InitAnimation, 0)
          FlipH = flipH |> Option.defaultValue false
          FlipV = false
          LastFrameTime = time
          FrameLength = aniConfig.FrameLength
          ScreenPos = pos }

//TODO: almost definitely wrong lol
let reInit (sprite: Model) (config: SpriteConfig) (curr: int32 voption) =
    match config with
    | SingleSpriteConfig singleConfig ->
        { sprite with
            Images = [ singleConfig.Image ]
            CurrentImage = singleConfig.Image
            Tint = singleConfig.Tint
            RelativeYPos = curr |> ValueOption.defaultValue sprite.RelativeYPos 
            AnimationState = Stopped(imageSpriteConfig, 0)
            FrameLength = singleConfig.FrameLength }

    | AnimatedSpriteConfig aniConfig ->
        let (img, yPos) =
            currentImageConfigAndRelativePos aniConfig.Images aniConfig.InitAnimation None

        { sprite with
            Images = aniConfig.Images
            CurrentImage = img
            Tint = aniConfig.Tint
            RelativeYPos = yPos
            AnimationState =
                if aniConfig.Started then
                    Started(aniConfig.InitAnimation, 0)
                else
                    Stopped(aniConfig.InitAnimation, 0) }

type Events =
    | None
    | AnimationComplete of int
    | AnimationLooped of int

let spriteSourceRect (spriteInfo: ImageConfig) (aniState: AnimationState) pos =
    let struct (width, height) = spriteInfo.SpriteSize

    let struct (xPos, yPos) =
        match aniState with
        | Stopped(_, x) -> x, pos
        | Started(_, x) -> x, pos

    let struct (x, y) = xPos * width, yPos * height
    rect x y width height

let viewSprite (model: Model) (cameraPos: Vector2) (loadedAssets: LoadedAssets) (spriteBatch: SpriteBatch) (depth: float32) =

    let texture = loadedAssets.textures.[model.CurrentImage.TextureName]
    //by convention the flipped sprite will be above the unflipped one
    let flipV = if model.FlipV then -1 else 0

    let sourceRect =
        spriteSourceRect model.CurrentImage model.AnimationState (model.RelativeYPos + flipV)

    let spriteCenter =
        Vector2(float32 (sourceRect.Width / 2), float32 (sourceRect.Height / 2))

    let cameraOffset = -cameraPos

    let actualX = int (model.ScreenPos.X + cameraOffset.X)
    let actualY = int (model.ScreenPos.Y + cameraOffset.Y)

    spriteBatch.Draw(
        texture,
        Rectangle(actualX, actualY, sourceRect.Width, sourceRect.Height),
        sourceRect,
        model.Tint,
        0f,
        spriteCenter + model.CurrentImage.Offset,
        (if model.FlipH then
             Graphics.SpriteEffects.FlipHorizontally
         else
             Graphics.SpriteEffects.None),
        depth
    )


let animTick time model =
    let t = time - model.LastFrameTime

    let struct (t, inc) =
        match (t > model.FrameLength) with
        | true -> struct (time, 1)
        | false -> struct (model.LastFrameTime, 0)

    let struct (newAnimationState, event) =
        match model.AnimationState with
        | Stopped(ani, oldX) -> Stopped(ani, oldX), Events.None
        | Started(ani, oldX) ->
            let nextX = oldX + inc

            if ani.Looping then
                let x = ((oldX + inc) % ani.Columns)

                let event =
                    if x = 0 then
                        Events.AnimationLooped ani.Index
                    else
                        Events.None

                Started(ani, x), event
            else
                let lastFrame = ani.Columns - 1

                if nextX > oldX && nextX = lastFrame then
                    Started(ani, lastFrame), Events.AnimationComplete ani.Index
                else if nextX > lastFrame then
                    Started(ani, lastFrame), Events.None
                else
                    Started(ani, nextX), Events.None

    struct ({ model with
                AnimationState = newAnimationState
                LastFrameTime = t },
            event)

let stop model =
    match model.AnimationState with
    | Stopped _ -> model
    | Started(a, b) -> { model with AnimationState = Stopped(a, b) }

let switchAnimation (newAni:AnimationConfig, increment, start) model =
    let (img, yPos) = currentImageConfigAndRelativePos model.Images newAni Option.None

    { model with
        AnimationState = if start then Started(newAni, 0) else Stopped(newAni, 0)
        CurrentImage = img
        RelativeYPos = yPos
        FrameLength = increment }

let startAnimation model =
    match model.AnimationState with
    | Started _ -> model
    | Stopped(a, b) -> { model with AnimationState = Started(a, b) }

let setPos p model = { model with ScreenPos = p }
let setDirectionX flipH model = { model with FlipH = flipH }
let setDirectionY flipV model = { model with FlipV = flipV }
