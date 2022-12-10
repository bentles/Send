[<RequireQualifiedAccess>]
module Player

open Elmish
open Xelmish.Viewables
open Xelmish.Model
open Microsoft.Xna.Framework
open Input
open Debug
open Config

type CharacterState =
    | Small of bool
    | Growing
    | Shrinking

type Model =
    { SpriteInfo: Sprite.Model
      Pos: Vector2
      Input: Vector2

      CharacterState: CharacterState
      Acc: float32
      MaxVelocity: float32
      Friction: float32

      Vel: Vector2 }


let init x y (playerConfig: PlayerConfig) spriteConfig =
    let p = Vector2(float32 x, float32 y)

    { SpriteInfo = Sprite.init p spriteConfig

      CharacterState = Small true


      Pos = p
      Vel = Vector2.Zero

      MaxVelocity = playerConfig.MaxVelocity
      Friction = playerConfig.Slow
      Acc = playerConfig.Acc

      Input = Vector2.Zero }

type Message =
    | Move of dir: Vector2
    | TransformCharacter
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

let physics model time = //past pos and return new pos
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

    let directionCommands =
        if sign (vel.X) <> sign (model.Vel.X) && vel.X <> 0f then
            [ Cmd.ofMsg (SpriteMessage(Sprite.SetDirection(vel.X < 0f))) ]
        else
            []

    // todo only send a command if the velocity goes above/below a treshold
    let oldVelLength = model.Vel.Length()

    let animationCommands =
        match (oldVelLength, velLength, model.CharacterState) with
        | (0f, v, Small isSmall) when v > 0f ->
            let walkAnimation =
                match isSmall with
                | true -> CharAnimations.SmallWalk
                | false -> CharAnimations.BigWalk

            let switchToWalkAnimation =
                (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (walkAnimation, 80)

            let startWalkAnimation = (Cmd.ofMsg << SpriteMessage) Sprite.StartAnimation
            [ switchToWalkAnimation; startWalkAnimation ] //do this on X changing
        | (ov, 0f, Small _) when ov > 0f -> [ Cmd.ofMsg (SpriteMessage(Sprite.Stop)) ]
        | (_, _, _) -> []

    //every 75 pixels is 1m
    let pixelsPerMeter = 75f

    //pos affected by velocity
    let pos =
        Vector2.Add(model.Pos, Vector2.Multiply(Vector2.Multiply(vel, dt), pixelsPerMeter))

    let setPosMsg = Cmd.ofMsg (SpriteMessage(Sprite.SetPos pos))

    { model with Vel = vel; Pos = pos }, Cmd.batch [ setPosMsg; yield! animationCommands; yield! directionCommands ]

let getTransformAnimation (characterState: CharacterState) =
    match characterState with
    | Shrinking
    | Small false -> CharAnimations.SmallToBig
    | Growing
    | Small true -> CharAnimations.BigToSmall

let getTransform (characterState: CharacterState) =
    match characterState with
    | Shrinking
    | Small true -> Growing
    | Growing
    | Small false -> Shrinking

let getCompletionState (characterState: CharacterState) =
    match characterState with
    | Shrinking
    | Small true -> Small true
    | Growing
    | Small false -> Small false

let getWalkAnimation (characterState: CharacterState) : AnimationConfig option =
    match characterState with
    | Small true -> Some CharAnimations.SmallWalk
    | Small false -> Some CharAnimations.BigWalk
    | _ -> None

let update message model =
    match message with
    | Move dir -> { model with Input = dir }, Cmd.none
    | PhysicsTick time -> physics model time
    | SpriteMessage sm ->
        let (newSprite, event) = Sprite.update sm model.SpriteInfo

        let model, cmd =
            match event with
            | Sprite.AnimationComplete _ -> 
                let modl = {model with CharacterState = getCompletionState model.CharacterState}
                let maybeWalkAni = getWalkAnimation modl.CharacterState
                match maybeWalkAni with
                  | None -> modl, Cmd.none
                  | Some ani -> modl, (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (ani, 80)                
            | Sprite.None -> model, Cmd.none

        { model with SpriteInfo = newSprite }, cmd
    | TransformCharacter ->
        let transformAnimation = getTransformAnimation model.CharacterState

        { model with CharacterState = getTransform model.CharacterState },
        Cmd.batch
            [ (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (transformAnimation, 80)
              (Cmd.ofMsg << SpriteMessage) Sprite.StartAnimation ]

let view model (cameraPos: Vector2) (dispatch: Message -> unit) =
    [ yield! Sprite.view model.SpriteInfo (cameraPos: Vector2) (SpriteMessage >> dispatch)
      yield debugText $"X:{model.Pos.X} \nY:{model.Pos.Y}" (10, 200)
      yield onupdate (fun input -> dispatch (PhysicsTick input.totalGameTime))

      yield directions Keys.Up Keys.Down Keys.Left Keys.Right (fun f -> dispatch (Move f))
      yield onkeydown Keys.Z (fun f -> dispatch (TransformCharacter)) ]
