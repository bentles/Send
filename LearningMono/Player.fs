[<RequireQualifiedAccess>]
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

type CollisionInfo = {
    //collision
    Half: Vector2
    Offset: Vector2
}

type Model =
    { SpriteInfo: Sprite.Model
      CharacterState: CharacterState
      Input: Vector2

      Carrying: Entity.Model list

      //physics
      Pos: Vector2
      Acc: float32
      MaxVelocity: float32
      Friction: float32
      Vel: Vector2
      IsMoving: bool 
      
      CollisionInfo: CollisionInfo
      }

let init x y (playerConfig: PlayerConfig) (spriteConfig:SpriteConfig) =
    let p = Vector2(float32 x, float32 y)

    { SpriteInfo = Sprite.init p spriteConfig
      CharacterState = Small true
      Input = Vector2.Zero
      Carrying = [
        Entity.initNoCollider observerSpriteConfig p 
        Entity.initNoCollider observerSpriteConfig p 
        Entity.initNoCollider observerSpriteConfig p 
        Entity.initNoCollider observerSpriteConfig p 
        Entity.initNoCollider observerSpriteConfig p 
        Entity.initNoCollider observerSpriteConfig p 
        Entity.initNoCollider observerSpriteConfig p 
        Entity.initNoCollider observerSpriteConfig p 
        Entity.initNoCollider observerSpriteConfig p 
      ]

      Pos = p
      MaxVelocity = playerConfig.SmallMaxVelocity
      Acc = playerConfig.Acc
      Friction = playerConfig.Slow
      Vel = Vector2.Zero
      IsMoving = false 
      CollisionInfo = {
          Half = playerConfig.AABBConfig.Half
          Offset = playerConfig.AABBConfig.Offset
      }
    }

type PhysicsInfo =
    { Time: int64
      PossibleObstacles: AABB seq }

type Message =
    | Input of dir: Vector2
    | TransformCharacter
    | PhysicsTick of info: PhysicsInfo
    | SpriteMessage of Sprite.Message
    | CarryingMessage of Sprite.Message

let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let collider (pos: Vector2) (collisionInfo:CollisionInfo) : AABB = { Pos = pos + collisionInfo.Offset; Half = collisionInfo.Half }

let calcVelocity modelVel modelMaxVel (acc: Vector2) (dt: float32) =
    let vel = modelVel + acc * dt

    //no osciallating weirdness if you stop you stop
    let stopped = Vector2.Dot(vel, modelVel) < 0f
    let vel = if stopped then Vector2.Zero else vel
    let velLength = vel.Length()

    let velTooBig = velLength > modelMaxVel

    let vel =
        if velTooBig then
            Vector2.Normalize(vel) * modelMaxVel
        else
            vel

    vel, velLength

let collide pos oldPos colInfo obstacles =
    let sweepIntoWithOffset pos oldPos obstacles = 
        let deltaPos = pos - oldPos
        let sweepResult = sweepInto (collider oldPos colInfo) obstacles deltaPos
        let result = { sweepResult with Pos = sweepResult.Pos - colInfo.Offset }
        
        //collision distance should be <= unadjusted distance
        assert ((result.Pos - oldPos).Length() <= deltaPos.Length() + AcceptableError)
        result


    if Seq.isEmpty obstacles then
        pos
    else        
        let sweep1 = sweepIntoWithOffset pos oldPos obstacles

        match sweep1.Hit with
        | Some hit -> 
            let movementIntoAABB = pos - sweep1.Pos
            let vectorOut = (hit.Normal * hit.Normal) * movementIntoAABB //grab the component that points out
            let deltaParallel = movementIntoAABB - vectorOut //calc component along the surface

            if deltaParallel = Vector2.Zero then
                sweep1.Pos
            else
                // collide again
                let sweep2 = sweepIntoWithOffset (sweep1.Pos + deltaParallel) sweep1.Pos obstacles 

                match sweep2.Hit with
                    | Some hit2 -> sweep2.Pos
                    | None -> sweep1.Pos + deltaParallel

        | None -> pos

let inputAffectsVelocityAssertions (input:Vector2) (oldVel:Vector2) (newVel:Vector2) : bool = 
    if input = Vector2.Zero then
        newVel.Length() <= oldVel.Length() + AcceptableError
    else
        Vector2.Dot(input, newVel) >= Vector2.Dot(input, newVel) - AcceptableError

let updateCarryingPositions (carrying: Entity.Model list) (pos:Vector2) (charState: CharacterState) =
    Cmd.ofMsg (CarryingMessage (Sprite.Message.SetPos pos))
    

let physics model (info: PhysicsInfo) =
    let dt = (float32 (info.Time - lastTick)) / 1000f
    lastTick <- info.Time

    let acc =
        match (model.Input, model.Vel) with
        | (i, v) when i = Vector2.Zero && v = Vector2.Zero -> Vector2.Zero
        | (i, v) when i = Vector2.Zero -> //slow down against current velocity
            Vector2.Normalize(v) * -(model.Friction)
        | (i, _) -> i * float32 (model.Acc)

    let (vel, velLength) = calcVelocity model.Vel model.MaxVelocity acc dt

    assert (inputAffectsVelocityAssertions model.Input model.Vel vel)

    //BlockWidth pixels is 1m
    let pixelsPerMeter = float32 worldConfig.TileWidth

    let preCollisionPos =
        model.Pos + (vel * dt) * pixelsPerMeter

    //collide with walls
    let pos = collide preCollisionPos model.Pos model.CollisionInfo info.PossibleObstacles

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

    let carryCommands = updateCarryingPositions newModel.Carrying newModel.Pos newModel.CharacterState

    Cmd.batch [ setPosMsg; carryCommands; yield! animationCommands; yield! directionCommands ]

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
    | Input direction -> { model with Input = direction }, Cmd.none
    | PhysicsTick info ->
        let newModel = physics model info
        let aniCommands = animations newModel model
        newModel, aniCommands
    | SpriteMessage sm ->
        let (newSprite, event) = Sprite.update sm model.SpriteInfo

        let model, cmd =
            match event with
            | Sprite.AnimationComplete _ ->
                let (newState, walkAni) = transformComplete model.CharacterState
                let maxVelocity = 
                    match newState with
                    | Small s when s -> playerConfig.SmallMaxVelocity
                    | Small s when not s -> playerConfig.BigMaxVelocity
                    | _ -> model.MaxVelocity
                let modl = { model with CharacterState = newState; MaxVelocity = maxVelocity }
                modl, (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (walkAni, 80, modl.IsMoving)
            | Sprite.None -> model, Cmd.none

        { model with SpriteInfo = newSprite }, cmd
    | CarryingMessage sm ->
        let newCarrying = 
            model.Carrying |> List.mapi (fun i  carry -> 
                let (newSprite, _) = Sprite.update sm carry.Sprite
                { carry with Sprite = newSprite }
            )
        { model with Carrying = newCarrying }, Cmd.none
    | TransformCharacter ->
        let (newState, transformAnimation) = transformStart model.CharacterState

        { model with CharacterState = newState },
        (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (transformAnimation, 80, true)

let renderAABB (aabb: AABB) (cameraPos:Vector2) =
    image
        "tile"
        Color.Red
        (int (aabb.Half.X * 2f), int (aabb.Half.Y * 2f))
        (int (aabb.Pos.X - aabb.Half.X - cameraPos.X), int (aabb.Pos.Y - aabb.Half.Y - cameraPos.Y))

let renderCarrying (carrying:Entity.Model list) (cameraPos:Vector2) (charState: CharacterState) = 
    let offsetStart =
        match charState with 
        | Small true -> Vector2(0f, 40f) 
        | Small false -> Vector2(0f, 70f) 
        | _ -> Vector2(0f, 55f)

    carrying |> 
        List.indexed |>
        List.collect (fun (i, c) -> 
            let offSetPos = cameraPos + offsetStart + (Vector2(0f, 25f) * (float32 i))
            Sprite.view c.Sprite offSetPos (fun f -> ()) )

let view model (cameraPos: Vector2) (dispatch: Message -> unit) =
    [

      //render
      yield! Sprite.view model.SpriteInfo cameraPos (SpriteMessage >> dispatch)
      yield! renderCarrying model.Carrying cameraPos model.CharacterState

      //debug
      yield debugText $"X:{model.Pos.X} \nY:{model.Pos.Y}" (10, 200)
      //yield renderAABB (collider model.Pos model.CollisionInfo) cameraPos

      //IO
      yield directions Keys.Up Keys.Down Keys.Left Keys.Right (fun f -> dispatch (Input f))
      yield onkeydown Keys.Space (fun f -> dispatch (TransformCharacter)) ]
