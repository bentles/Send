﻿[<RequireQualifiedAccess>]
module Player

open Elmish
open Xelmish.Viewables
open Xelmish.Model
open Microsoft.Xna.Framework
open Input
open Debug
open Config
open Collision

type CharacterState =
    | Small of bool
    | Growing
    | Shrinking

type Model =
    { SpriteInfo: Sprite.Model
      CharacterState: CharacterState
      Input: Vector2

      //physics
      Pos: Vector2
      Acc: float32
      MaxVelocity: float32
      Friction: float32
      Vel: Vector2
      IsMoving: bool }

let init x y (playerConfig: PlayerConfig) spriteConfig =
    let p = Vector2(float32 x, float32 y)

    { SpriteInfo = Sprite.init p spriteConfig
      CharacterState = Small true
      Input = Vector2.Zero

      Pos = p
      MaxVelocity = playerConfig.MaxVelocity
      Acc = playerConfig.Acc
      Friction = playerConfig.Slow
      Vel = Vector2.Zero
      IsMoving = false }

type Message =
    | Move of dir: Vector2
    | TransformCharacter
    | PhysicsTick of time: int64
    | SpriteMessage of Sprite.Message

let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let collider (pos: Vector2): AABB = 
    {
        Pos = pos
        Half = Vector2(50f, 52f)
    }

let calcVelocity modelVel modelMaxVel (acc: Vector2) (dt: float32) =
    let vel = Vector2.Add(modelVel, Vector2.Multiply(acc, dt))

    //no osciallating weirdness if you stop you stop
    let stopped = Vector2.Dot(vel, modelVel) < 0f
    let vel = if stopped then Vector2.Zero else vel
    let velLength = vel.Length()

    let velTooBig = velLength > modelMaxVel
    let vel = 
        if velTooBig then
            Vector2.Multiply(Vector2.Normalize(vel), modelMaxVel)
        else
            vel

    vel, velLength

let physics model time =
    let dt = (float32 (time - lastTick)) / 1000f
    lastTick <- time

    let acc =
        match (model.Input, model.Vel) with
        | (i, v) when i = Vector2.Zero && v = Vector2.Zero -> Vector2.Zero
        | (i, v) when i = Vector2.Zero -> //slow down against current velocity
            Vector2.Multiply(Vector2.Normalize(v), -(model.Friction))
        | (i, _) -> Vector2.Multiply(i, float32 (model.Acc))

    let (vel, velLength) = calcVelocity model.Vel model.MaxVelocity acc dt

    //every 75 pixels is 1m
    let pixelsPerMeter = 75f

    let pos =
        Vector2.Add(model.Pos, Vector2.Multiply(Vector2.Multiply(vel, dt), pixelsPerMeter))

    { model with
        Vel = vel
        Pos = pos
        IsMoving = velLength > 0f }

let animations newModel oldModel =
    let directionCommands =
        if sign (newModel.Vel.X) <> sign (oldModel.Vel.X) && newModel.Vel.X <> 0f then
            [ Cmd.ofMsg (SpriteMessage(Sprite.SetDirection(newModel.Vel.X < 0f))) ]
        else
            []

    let animationCommands =
        match (oldModel.IsMoving, newModel.IsMoving, oldModel.CharacterState) with
        | (false, true, Small isSmall) ->
            let walkAnimation =
                match isSmall with
                | true -> CharAnimations.SmallWalk
                | false -> CharAnimations.BigWalk

            [ (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (walkAnimation, 80, true) ]
        | (true, false, Small _) -> [ (Cmd.ofMsg << SpriteMessage) Sprite.Stop ]
        | _ -> []

    let setPosMsg = Cmd.ofMsg (SpriteMessage(Sprite.SetPos newModel.Pos))
    Cmd.batch [ setPosMsg; yield! animationCommands; yield! directionCommands ]

let transformStart (characterState: CharacterState) =
    match characterState with
    | Shrinking -> Growing, CharAnimations.SmallToBig
    | Small true -> Growing, CharAnimations.BigToSmall
    | Growing -> Shrinking, CharAnimations.BigToSmall
    | Small false -> Shrinking, CharAnimations.SmallToBig

let transformComplete (characterState: CharacterState) =
    match characterState with
    | Shrinking -> Small true, CharAnimations.SmallWalk
    | Growing -> Small false, CharAnimations.BigWalk
    | Small true -> Small true, CharAnimations.SmallWalk
    | Small false -> Small false, CharAnimations.BigWalk

let update message model =
    match message with
    | Move dir -> { model with Input = dir }, Cmd.none
    | PhysicsTick time ->
        let newModel = physics model time
        let aniCommands = animations newModel model
        newModel, aniCommands
    | SpriteMessage sm ->
        let (newSprite, event) = Sprite.update sm model.SpriteInfo
        let model, cmd =
            match event with
            | Sprite.AnimationComplete _ ->
                let (newState, walkAni) = transformComplete model.CharacterState
                let modl = { model with CharacterState = newState }
                modl, (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (walkAni, 80, modl.IsMoving)       
            | Sprite.None -> model, Cmd.none
        { model with SpriteInfo = newSprite }, cmd
    | TransformCharacter ->
        let (newState, transformAnimation) = transformStart model.CharacterState

        { model with CharacterState = newState },
        (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (transformAnimation, 80, true)

let view model (cameraPos: Vector2) (dispatch: Message -> unit) =
    [ 
      //render  
      yield! Sprite.view model.SpriteInfo (cameraPos: Vector2) (SpriteMessage >> dispatch)
      yield debugText $"X:{model.Pos.X} \nY:{model.Pos.Y}" (10, 200)
      
      //physics
      yield onupdate (fun input -> dispatch (PhysicsTick input.totalGameTime))

      //IO
      yield directions Keys.Up Keys.Down Keys.Left Keys.Right (fun f -> dispatch (Move f))
      yield onkeydown Keys.Z (fun f -> dispatch (TransformCharacter)) ]
