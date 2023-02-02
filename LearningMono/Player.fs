[<RequireQualifiedAccess>]
module Player

open Microsoft.Xna.Framework
open Prelude
open Collision
open Level
open Entity
open GameConfig
open PlayerConfig
open Elmish
open Utility

type State =
    | Small of bool
    | Growing
    | Shrinking

type Model =
    { SpriteInfo: Sprite.Model
      CharacterState: State

      Input: Vector2
      XInputTimeAndDir: int64 * float32
      YInputTimeAndDir: int64 * float32

      MovementFrozen: bool
      ArrowsControlPlacement: bool

      Carrying: Entity.Model list
      Target: Vector2
      PlacementFacing: Facing

      //physics
      Facing: Vector2
      Pos: Vector2
      Acc: float32
      MaxVelocity: float32
      Friction: float32
      Vel: Vector2
      IsMoving: bool

      CollisionInfo: CollisionInfo }

let init x y (playerConfig: PlayerConfig) (spriteConfig: SpriteConfig) time =
    let p = Vector2(float32 x, float32 y)

    { SpriteInfo = Sprite.init p time spriteConfig None None
      CharacterState = Small true
      Input = Vector2.Zero
      XInputTimeAndDir = -1000, 1f
      YInputTimeAndDir = 0, 0f
      PlacementFacing = FacingRight

      MovementFrozen = false
      ArrowsControlPlacement = false

      Carrying =
          [ Entity.initNoCollider (buildObserver (Filter(rockTimer))) p time FacingRight
            Entity.initNoCollider (buildObserver (Map(rockTimer))) p time FacingRight
            Entity.initNoCollider (buildObserver (Filter Rock)) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight ]
      Facing = Vector2(1f, 0f)
      Target = p + 60f * Vector2(1f, 0f)
      Pos = p
      MaxVelocity = playerConfig.SmallMaxVelocity
      Acc = playerConfig.Acc
      Friction = playerConfig.Slow
      Vel = Vector2.Zero
      IsMoving = false
      CollisionInfo =
        { Half = playerConfig.AABBConfig.Half
          Offset = playerConfig.AABBConfig.Pos } }


let getPlayerPickupLimit (characterState: State) =
    match characterState with
    | Small true -> 15
    | Small false -> 15
    | _ -> 0


type Message =
    | Input of dir: Vector2
    | TransformCharacter
    | FreezeMovement of bool
    | ArrowsControlPlacement of bool
    | RotatePlacement of clockwise: bool
    | PlayerPhysicsTick of info: PhysicsInfo
    | SpriteMessage of Sprite.Message
    | CarryingMessage of Sprite.Message

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


let updateCarryingPositions (pos: Vector2) =
    Cmd.ofMsg (CarryingMessage(Sprite.Message.SetPos pos))

let updatePhysics (model: Model) (info: PhysicsInfo) =
    let dt = info.Dt
    let currentTime = info.Time

    let updateIfNotZero (value: float32) (prev: int64 * float32) =
        if value <> 0f then currentTime, value else prev

    let calcFacing (milisSinceX, lastXDir) (milisSinceY, lastYDir) =
        // if both keys are released within minTime of each other we are facing diagonally
        let diagonal = abs (milisSinceX - milisSinceY) < diagonalReleaseDelay

        let facing =
            match (model.Input.X, model.Input.Y) with
            | (0f, 0f) when diagonal -> Vector2(lastXDir, lastYDir)
            | (0f, 0f) when milisSinceX < milisSinceY -> Vector2(lastXDir, 0f)
            | (0f, 0f) when milisSinceY <= milisSinceX -> Vector2(0f, lastYDir)

            | (0f, y) when milisSinceX < diagonalReleaseDelay -> Vector2(lastXDir, y)
            | (x, 0f) when milisSinceY < diagonalReleaseDelay -> Vector2(x, lastYDir)
            | _ -> model.Input

        Vector2.Normalize(facing)

    let millisSince eventTime =
        (float32 (currentTime - eventTime)) / 1000f

    // acceleration and velocity
    let acc =
        match (model.Input, model.Vel) with
        | (i, v) when i = Vector2.Zero && v = Vector2.Zero -> Vector2.Zero
        | (i, v) when i = Vector2.Zero -> //slow down against current velocity
            Vector2.Normalize(v) * -(model.Friction)
        | (i, _) -> i * float32 (model.Acc)

    let (vel, velLength) = calcVelocity model.Vel model.MaxVelocity acc dt

    assert (Assert.inputAffectsVelocityAssertions model.Input model.Vel vel)

    // TileWidth pixels is 1m
    let pixelsPerMeter = float32 worldConfig.TileWidth

    let preCollisionPos = model.Pos + (vel * dt) * pixelsPerMeter

    // collide with walls
    let pos =
        collide preCollisionPos model.Pos model.CollisionInfo info.PossibleObstacles

    // record when last x and y were pressed
    let xinputTime, lastXDir = updateIfNotZero model.Input.X model.XInputTimeAndDir
    let yinputTime, lastYDir = updateIfNotZero model.Input.Y model.YInputTimeAndDir
    let milisSinceX = millisSince xinputTime
    let milisSinceY = millisSince yinputTime

    let facing = calcFacing (milisSinceX, lastXDir) (milisSinceY, lastYDir)

    let target = pos + (60f * facing) + Vector2(0f, 20f)

    let (vel, pos, isMoving) =
        if model.MovementFrozen then
            (Vector2.Zero, model.Pos, false)
        else
            (vel, pos, velLength > 0f)

    let placementFacing =
        vectorToFacing model.Input |> Option.defaultValue model.PlacementFacing

    { model with
        Target =
            if model.ArrowsControlPlacement then
                model.Target
            else
                target
        XInputTimeAndDir = xinputTime, lastXDir
        YInputTimeAndDir = yinputTime, lastYDir
        Facing =
            if model.ArrowsControlPlacement then
                model.Facing
            else
                facing
        PlacementFacing =
            if model.ArrowsControlPlacement then
                placementFacing
            else
                model.PlacementFacing
        Vel = vel
        Pos = pos
        IsMoving = isMoving }

let updateAnimations (newModel: Model) (oldModel: Model) =
    let directionCommands =
        [ if newModel.Facing.X <> 0f && newModel.Facing.X <> oldModel.Facing.X then
              Cmd.ofMsg (SpriteMessage(Sprite.SetDirectionX(newModel.Facing.X < 0f)))
          if newModel.Facing.Y <> 0f && newModel.Facing.Y <> oldModel.Facing.Y then
              Cmd.ofMsg (SpriteMessage(Sprite.SetDirectionY(newModel.Facing.Y < 0f))) ]

    let animationCommands =
        match (oldModel.IsMoving, newModel.IsMoving, oldModel.CharacterState) with
        | (false, true, Small isSmall) ->
            let walkAnimation, speed =
                match isSmall with
                | true -> CharAnimations.SmallWalk, CharConfig.BigFrames
                | false -> CharAnimations.BigWalk, CharConfig.SmallFrames

            [ (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (walkAnimation, speed, true) ]
        | (true, false, Small _) -> [ (Cmd.ofMsg << SpriteMessage) Sprite.Stop ]
        | _ -> []

    let setPosMsg = Cmd.ofMsg (SpriteMessage(Sprite.SetPos newModel.Pos))

    let carryCommand = updateCarryingPositions newModel.Pos

    Cmd.batch [ setPosMsg; carryCommand; yield! animationCommands; yield! directionCommands ]

let transformStart (characterState: State) =
    match characterState with
    | Shrinking -> Growing, CharAnimations.SmallToBig
    | Small true -> Growing, CharAnimations.SmallToBig
    | Growing -> Shrinking, CharAnimations.BigToSmall
    | Small false -> Shrinking, CharAnimations.BigToSmall

let transformComplete (characterState: State) =
    match characterState with
    | Shrinking -> Small true, CharAnimations.SmallWalk
    | Growing -> Small false, CharAnimations.BigWalk
    | Small true -> Small true, CharAnimations.SmallWalk
    | Small false -> Small false, CharAnimations.BigWalk

let viewCarrying (carrying: Entity.Model list) (cameraPos: Vector2) (charState: State) =
    let offsetStart =
        match charState with
        | Small true -> Vector2(0f, 40f)
        | Small false -> Vector2(0f, 70f)
        | _ -> Vector2(0f, 55f)

    carrying
    |> Seq.indexed
    |> Seq.collect (fun (i, c) ->
        let offSetPos = cameraPos + offsetStart + (Vector2(0f, 25f) * (float32 i))
        Sprite.view c.Sprite offSetPos ignore)
