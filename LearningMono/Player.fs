[<RequireQualifiedAccess>]
module Player

open Microsoft.Xna.Framework
open Prelude
open Collision
open Entity
open GameConfig
open PlayerConfig
open Utility
open Xelmish.Model

type State =
    | Small of bool
    | Growing
    | Shrinking

[<Struct>]
type MultiPlace =
    { LastTime: int64
      Coords: Coords
      Facing: Facing }

type PlayerTarget = voption<struct (Level.Tile * int)>

type Model =
    { SpriteInfo: Sprite.Model
      CharacterState: State

      Input: Vector2
      XInputTimeAndDir: int64 * float32
      YInputTimeAndDir: int64 * float32

      MovementFrozen: bool
      ArrowsControlPlacement: bool

      Carrying: Entity.Model list
      CarryingDelta: int
      Target: Vector2
      PlacementFacing: Facing
      MultiPlace: MultiPlace voption

      Action: PlayerWorldInteraction      
      TargetedTile: PlayerTarget
      Feet: int voption

      //physics
      Facing: Vector2
      Pos: Vector2
      Acc: float32
      MaxVelocity: float32
      Friction: float32
      Vel: Vector2
      IsMoving: bool }

let init (pos: Vector2) (carrying: Entity.Model list) (playerConfig: PlayerConfig) (spriteConfig: SpriteConfig) time =
    { SpriteInfo = Sprite.init pos time spriteConfig None None
      CharacterState = Small true
      Input = Vector2.Zero
      XInputTimeAndDir = 0, 1f
      YInputTimeAndDir = 0, 0f
      PlacementFacing = FacingRight

      MovementFrozen = false
      ArrowsControlPlacement = false
      MultiPlace = ValueNone

      Carrying = carrying
      CarryingDelta = 0

      Action = NoAction
      TargetedTile = ValueNone
      Feet = ValueNone

      Facing = Vector2(1f, 0f)
      Target = pos + 55f * Vector2(1f, 0f)
      Pos = pos
      MaxVelocity = playerConfig.SmallMaxVelocity
      Acc = playerConfig.Acc
      Friction = playerConfig.Slow
      Vel = Vector2.Zero
      IsMoving = false }


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

    let millisSince eventTime = (currentTime - eventTime)

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
    let pixelsPerMeter = float32 WorldConfig.TileWidth

    let preCollisionPos = model.Pos + (vel * dt) * pixelsPerMeter

    // collide with walls
    let pos = collide preCollisionPos model.Pos info.PossibleObstacles

    // record when last x and y were pressed
    let xinputTime, lastXDir = updateIfNotZero model.Input.X model.XInputTimeAndDir
    let yinputTime, lastYDir = updateIfNotZero model.Input.Y model.YInputTimeAndDir
    let milisSinceX = millisSince xinputTime
    let milisSinceY = millisSince yinputTime

    let facing = calcFacing (milisSinceX, lastXDir) (milisSinceY, lastYDir)
    let target = pos + (55f * facing)

    let (vel, pos, isMoving) =
        if model.MovementFrozen then
            (Vector2.Zero, model.Pos, false)
        else
            (vel, pos, velLength > 0f)

    let placementFacing =
        vectorToFacing model.Input |> ValueOption.defaultValue model.PlacementFacing

    if model.ArrowsControlPlacement then
        { model with
            PlacementFacing = placementFacing
            Vel = vel
            Pos = pos
            IsMoving = isMoving }
    else
        { model with
            Target = target
            XInputTimeAndDir = xinputTime, lastXDir
            YInputTimeAndDir = yinputTime, lastYDir
            Facing = facing
            Vel = vel
            Pos = pos
            IsMoving = isMoving }

let changeDir facing oldFacing setDirection =
    if facing <> 0f && facing <> oldFacing then
        setDirection (facing < 0f)
    else
        id

let carryingUpdate pos model =
    model.Carrying
    |> List.map (fun carry ->
        let newSprite = Sprite.setPos pos carry.Sprite
        { carry with Sprite = newSprite })

let endMultiPlace model = { model with MultiPlace = ValueNone }

let updateAnimations (newModel: Model) (oldModel: Model) =
    let xChange = changeDir newModel.Facing.X oldModel.Facing.X Sprite.setDirectionX
    let yChange = changeDir newModel.Facing.Y oldModel.Facing.Y Sprite.setDirectionY

    let animationCommands =
        match (oldModel.IsMoving, newModel.IsMoving, oldModel.CharacterState) with
        | (false, true, Small isSmall) ->
            let walkAnimation, speed =
                match isSmall with
                | true -> CharAnimations.SmallWalk, CharConfig.BigFrames
                | false -> CharAnimations.BigWalk, CharConfig.SmallFrames

            Sprite.switchAnimation (walkAnimation, speed, true)
        | (true, false, Small _) -> Sprite.stop
        | _ -> id

    let setPos = Sprite.setPos newModel.Pos

    let updateCarrying = carryingUpdate newModel.Pos
    let updateSprite = (setPos >> animationCommands >> xChange >> yChange)

    { newModel with
        Carrying = (updateCarrying newModel)
        SpriteInfo = (updateSprite newModel.SpriteInfo) }

let (|PlayerCanPlace|PlayerCantPlace|) (player: Model) =
    match player.CharacterState, player.Carrying with
    | Small _, placeEntity :: rest -> PlayerCanPlace((placeEntity, rest))
    | _ -> PlayerCantPlace

let (|PlayerCanPickup|PlayerCantPickup|) (player: Model) =
    let playerLimit = getPlayerPickupLimit player.CharacterState
    match player.CharacterState with
    | Small _ when player.Carrying.Length + 1 <= playerLimit -> PlayerCanPickup
    | _ -> PlayerCantPickup


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

let tickAnimations model time =
    let struct (newSprite, event) = Sprite.animTick time model.SpriteInfo

    match event with
    | Sprite.AnimationComplete _ ->
        let (newState, walkAni) = transformComplete model.CharacterState

        let maxVelocity =
            match newState with
            | Small s when s -> playerConfig.SmallMaxVelocity
            | Small s when not s -> playerConfig.BigMaxVelocity
            | _ -> model.MaxVelocity


        { model with
            CharacterState = newState
            MaxVelocity = maxVelocity
            SpriteInfo = Sprite.switchAnimation (walkAni, walkAni.Speed, model.IsMoving) newSprite }


    | Sprite.AnimationLooped _
    | Sprite.None -> { model with SpriteInfo = newSprite }

let tick (info: PhysicsInfo) (model: Model) : Model =
    let newModel = updatePhysics model info
    //possibly move animation stuff out of physics
    let newModel = updateAnimations newModel model
    let newModel = tickAnimations newModel info.Time
    newModel

let update (message: Message) (model: Model) =
    match message with
    | Input direction -> { model with Input = direction }
    | TransformCharacter ->
        let (newState, transformAnimation) = transformStart model.CharacterState

        { model with
            CharacterState = newState
            SpriteInfo = Sprite.switchAnimation (transformAnimation, 100, true) model.SpriteInfo }

    | FreezeMovement holding -> { model with MovementFrozen = holding }
    | ArrowsControlPlacement theyDo ->
        { model with
            MovementFrozen = theyDo
            ArrowsControlPlacement = theyDo }

let setAction (player:Model) (action:PlayerWorldInteraction) : Model =
    { player with Action = action }


let viewCarrying
    (carrying: Entity.Model list)
    (cameraPos: Vector2)
    (charState: State)
    (loadedAssets: LoadedAssets)
    (spriteBatch: Graphics.SpriteBatch)
    (depth: float32)
    =
    let offsetStart =
        match charState with
        | Small true -> Vector2(0f, 60f)
        | Small false -> Vector2(0f, 90f)
        | _ -> Vector2(0f, 75f)

    carrying
    |> Seq.iteri (fun i item ->
        let offSetPos = cameraPos + offsetStart + (Vector2(0f, 25f) * (float32 i))
        Sprite.viewSprite item.Sprite offSetPos loadedAssets spriteBatch (depth + (float32 i) * DepthConfig.DepthSubFactor))

let hearCarrying (carryingDelta: int) (loadedAssets: LoadedAssets) =
    match carryingDelta with
    | 1 -> loadedAssets.sounds["pickUp"].Play(0.5f, 0f, 0f) |> ignore
    | -1 -> loadedAssets.sounds["place"].Play(0.5f, 0f, 0f) |> ignore
    | _ -> ()

let viewPlayer (model: Model) (cameraPos: Vector2) loadedAssets (spriteBatch: SpriteBatch) =
    let playerDepth = (model.Pos.Y * DepthConfig.DepthFactor + DepthConfig.Entities_And_Player)
    Sprite.viewSprite model.SpriteInfo cameraPos loadedAssets spriteBatch playerDepth

    viewAABB (playerCollider model.Pos) cameraPos loadedAssets spriteBatch
    viewCarrying model.Carrying cameraPos model.CharacterState loadedAssets spriteBatch playerDepth
    hearCarrying model.CarryingDelta loadedAssets

let inputs (inputs: Inputs) (dispatch: Message -> unit) =
    let gamepadDirection = Controls.directionsGamePad inputs
    let keyboardDirection = Controls.directions Keys.Up Keys.Down Keys.Left Keys.Right inputs

    dispatch (Input (gamepadDirection + keyboardDirection)) //this gets normalised anyway

    if Controls.isKeyDown Keys.Space inputs then
        (dispatch TransformCharacter)

    if Controls.isKeyDown Keys.LeftControl inputs then
        (dispatch (FreezeMovement true))

    if Controls.isKeyUp Keys.LeftControl inputs then
        (dispatch (FreezeMovement false))

    if Controls.isKeyDown Keys.LeftShift inputs then
        (dispatch (ArrowsControlPlacement true))

    if Controls.isKeyUp Keys.LeftShift inputs then
        (dispatch (ArrowsControlPlacement false))
