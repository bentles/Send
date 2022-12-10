module Sprite

open Microsoft.Xna.Framework
open Xelmish.Model
open Elmish
open Xelmish.Viewables
open Config
open Debug

type AnimationState =
    | Stopped of AnimationConfig * int
    | Started of AnimationConfig * int

type Model =
    { Images: ImageConfig list

      CurrentImage: ImageConfig
      RelativeYPos: int

      AnimationState: AnimationState

      Tint: Color
      LastFrameTime: int64
      FrameLength: int64
      FlipH: bool
      ScreenPos: Vector2 }

let currentImageConfigAndRelativePos images (animation: AnimationConfig) =
    let y = animation.Index

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
    let (img, yPos) =
        currentImageConfigAndRelativePos config.Images config.InitAnimation

    { Images = config.Images

      CurrentImage = img
      RelativeYPos = yPos

      Tint = config.Tint
      AnimationState = Stopped(config.InitAnimation, 0)
      FlipH = false

      LastFrameTime = 0L
      FrameLength = 300L
      ScreenPos = pos }

type Message =
    | Stop
    | SwitchAnimation of AnimationConfig * int64
    | StartAnimation
 //   | RestartAnimation
    | AnimTick of int64
    | SetPos of Vector2
    | SetDirection of bool

type Events =
    | None
    | AnimationComplete of int

let spriteSourceRect (spriteInfo: ImageConfig) (aniState: AnimationState) pos =
    let totalWidth, totalHeight = spriteInfo.PixelSize

    let xPos, yPos =
        match aniState with
        | Stopped(ani, x) -> x, pos
        | Started(ani, x) -> x, pos

    let height = totalHeight / spriteInfo.Rows
    let width = totalWidth / spriteInfo.Columns
    let x, y = xPos * width, yPos * height
    rect x y width height


let drawSprite (model: Model) (cameraPos:Vector2): Viewable =
    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->

        let texture = loadedAssets.textures[model.CurrentImage.TextureName]

        let sourceRect = spriteSourceRect model.CurrentImage model.AnimationState model.RelativeYPos

        let spriteCenter =
            Vector2(float32 (sourceRect.Width / 2), float32 (sourceRect.Height / 2))

        let cameraOffset = -cameraPos

        let actualX = int (model.ScreenPos.X + cameraOffset.X)
        let actualY =  int (model.ScreenPos.Y + cameraOffset.Y)

        spriteBatch.Draw(
            texture,
            Rectangle(actualX, actualY, sourceRect.Width, sourceRect.Height),
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

    let (newAnimationState, event) =
        match model.AnimationState with
        | Stopped(ani, oldX) -> Stopped(ani, oldX), Events.None
        | Started(ani, oldX) ->
            let nextX = oldX + inc

            if ani.Looping then
                Started(ani, ((oldX + inc) % ani.Columns)), Events.None
            else
                let lastFrame = ani.Columns - 1

                if nextX > oldX && nextX = lastFrame then
                    Started(ani, lastFrame), Events.AnimationComplete ani.Index
                else if nextX > lastFrame then
                    Started(ani, lastFrame), Events.None
                else
                    Started(ani, nextX), Events.None

    { model with
        AnimationState = newAnimationState
        LastFrameTime = t },
    event


let update message model =
    match message with
    | Stop ->
        match model.AnimationState with
        | Stopped _ -> model
        | Started(a, b) -> { model with AnimationState = Stopped(a, b) }
        , Events.None
    | SwitchAnimation(newAni, increment) ->
        let (img, yPos) = currentImageConfigAndRelativePos model.Images newAni
        { model with
            AnimationState = Stopped(newAni, 0)
            CurrentImage = img
            RelativeYPos = yPos
            FrameLength = increment },
        Events.None
    | StartAnimation ->
        match model.AnimationState with
        | Started _ -> model
        | Stopped(a, b) -> { model with AnimationState = Started(a, b) }
        , Events.None
    | AnimTick dt -> animTick model dt
    | SetPos p -> { model with ScreenPos = p }, Events.None
    | SetDirection flipH -> { model with FlipH = flipH }, Events.None


let view model (cameraPos:Vector2) (dispatch: Message -> unit) =
    [ yield drawSprite model cameraPos

      yield debugText $"X:{model.AnimationState}" (10, 30)
      yield onupdate (fun input -> dispatch (AnimTick input.totalGameTime)) ]
