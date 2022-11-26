[<RequireQualifiedAccess>]
module Player

open Elmish
open Xelmish.Viewables
open Xelmish.Model
open Microsoft.Xna.Framework
open Input
open Debug
open Config

type SelectedPlayer =
    | BigChar
    | SmallChar

type PlayerSprites =
    { BigChar: SpriteConfig
      SmallChar: SpriteConfig }

let getWalkAnimation sprite =
    match sprite with
    | BigChar _ -> 1
    | SmallChar _ -> 0

let getJumpAnimation sprite = 2

type Model =
    { SpriteInfo: Sprite.Model
      PlayerSprites: PlayerSprites
      SelectedSprite: SelectedPlayer
      Pos: Vector2
      Input: Vector2

      Acc: float32
      MaxVelocity: float32
      Friction: float32

      Vel: Vector2 }

let getconfig s = //is there an idiomatic way to do this??
    match s with
    | BigChar -> bigCharSprite
    | SmallChar -> smallCharSprite

let init x y maxVelocity acc slow selectedSprite =
    let p = Vector2(float32 x, float32 y)
    let spriteConfig = getconfig selectedSprite

    { SpriteInfo = Sprite.init p spriteConfig
      SelectedSprite = selectedSprite
      PlayerSprites =
        { BigChar = bigCharSprite
          SmallChar = smallCharSprite }

      Pos = p
      Vel = Vector2.Zero

      MaxVelocity = maxVelocity
      Friction = slow
      Acc = acc

      Input = Vector2.Zero }

type Message =
    | Move of dir: Vector2
    | SwitchSprite
    | PhysicsTick of time: int64
    | SpriteMessage of Sprite.Message

let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let calcVelocity model (acc: Vector2) (dt: float32) =
    let vel = Vector2.Add(model.Vel, Vector2.Multiply(acc, dt))

    //no osciallating weirdness if you stop you stop
    let vel =
        if Vector2.Dot(vel, model.Vel) < 0f then
            Vector2.Zero
        else
            vel

    let velSize = vel.Length()

    // remove massive and tiny velocities
    let vel =
        match velSize with
        | v when v > model.MaxVelocity -> Vector2.Multiply(Vector2.Normalize(vel), model.MaxVelocity)
        | _ -> vel

    vel, velSize

let physics model time =
    let dt = (float32 (time - lastTick)) / 1000f
    lastTick <- time

    let acc =
        match (model.Input, model.Vel) with
        | (i, v) when i = Vector2.Zero && v = Vector2.Zero -> Vector2.Zero
        | (i, v) when i = Vector2.Zero -> //slow down against current velocity
            Vector2.Multiply(Vector2.Normalize(v), -(model.Friction))
        | (i, v) -> Vector2.Multiply(i, float32 (model.Acc))

    //velocity must be modified by acc
    let (vel, velLength) = calcVelocity model acc dt

    let dirCommands =
        if sign (vel.X) <> sign (model.Vel.X) && vel.X <> 0f then
            [ Cmd.ofMsg (SpriteMessage(Sprite.SetDir(vel.X < 0f))) ]
        else
            []

    // todo only send a command if the velocity goes above/below a treshold
    let oldVelLength = model.Vel.Length()

    let aniCommands =
        match (oldVelLength, velLength) with
        | (0f, v) when v > 0f ->
            let walkAnimation = getWalkAnimation model.SelectedSprite
            let animMessage = SpriteMessage(Sprite.Animate(walkAnimation, 80))
            [ Cmd.ofMsg animMessage ] //do this on X changing
        | (ov, 0f) when ov > 0f -> [ Cmd.ofMsg (SpriteMessage(Sprite.Stop(1))) ]
        | (_, _) -> []

    //every 75 pixels is 1m
    let pixelsPerMeter = 75f

    //pos affected by velocity
    let pos =
        Vector2.Add(model.Pos, Vector2.Multiply(Vector2.Multiply(vel, dt), pixelsPerMeter))

    let setPosMsg = Cmd.ofMsg (SpriteMessage(Sprite.SetPos pos))

    { model with Vel = vel; Pos = pos }, Cmd.batch [ setPosMsg; yield! aniCommands; yield! dirCommands ]

let update message model =
    match message with
    | Move dir -> { model with Input = dir }, Cmd.none
    | PhysicsTick time -> physics model time
    | SpriteMessage sm ->
        let (newSprite, cmd) = Sprite.update sm model.SpriteInfo
        { model with SpriteInfo = newSprite }, cmd
    | SwitchSprite ->
        match model.SelectedSprite with
        | BigChar ->
            { model with
                SelectedSprite = SmallChar
                SpriteInfo = Sprite.init model.Pos model.PlayerSprites.SmallChar },
            Cmd.none
        | SmallChar ->
            { model with
                SelectedSprite = BigChar
                SpriteInfo = Sprite.init model.Pos model.PlayerSprites.BigChar },
            Cmd.none


let view model (dispatch: Message -> unit) =
    [ yield! Sprite.view model.SpriteInfo (SpriteMessage >> dispatch)
      yield debugText $"X:{model.Vel.X} \nY:{model.Vel.Y}" (10, 10)
      yield onupdate (fun input -> dispatch (PhysicsTick input.totalGameTime))

      yield directions Keys.Up Keys.Down Keys.Left Keys.Right (fun f -> dispatch (Move f))
      yield onkeydown Keys.Z (fun f -> dispatch (SwitchSprite)) ]
