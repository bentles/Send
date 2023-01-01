﻿module World

open Xelmish.Model
open Xelmish.Viewables
open Microsoft.Xna.Framework
open Config
open Elmish
open Collision
open Debug
open Input
open FsToolkit.ErrorHandling

let coordsToPos (xx: float32) (yy: float32) (half: Vector2) =
    let startX = 0f
    let startY = 0f

    let xBlockOffSet = float32 (xx * float32 worldConfig.TileWidth)
    let yBlockOffSet = float32 (yy * float32 worldConfig.TileWidth)

    let actualX = startX + xBlockOffSet + half.X
    let actualY = startY + yBlockOffSet + half.Y

    Vector2(actualX, actualY)

let posToCoords (pos: Vector2) : (int * int) =
    let x = int (pos.X / float32 worldConfig.TileWidth)
    let y = int (pos.Y / float32 worldConfig.TileWidth)
    (x, y)

let posRounded (pos: Vector2) (worldConfig: WorldConfig) =
    let x =
        (floor (pos.X / float32 worldConfig.TileWidth)) * float32 worldConfig.TileWidth

    let y =
        (floor (pos.Y / float32 worldConfig.TileWidth)) * float32 worldConfig.TileWidth

    Vector2(x, y) + Vector2(float32 (worldConfig.TileWidth / 2))

let createColliderFromCoords (xx: float32) (yy: float32) (half: Vector2) =
    { Pos = coordsToPos xx yy half
      Half = half }

type FloorType =
    | Empty
    | Grass

type CharacterState =
    | Small of bool
    | Growing
    | Shrinking

type CollisionInfo =
    {
      //collision
      Half: Vector2
      Offset: Vector2 }

type PlayerModel =
    { SpriteInfo: Sprite.Model
      CharacterState: CharacterState
      Input: Vector2

      Carrying: Entity.Model list

      //physics
      Facing: Vector2
      Pos: Vector2
      Acc: float32
      MaxVelocity: float32
      Friction: float32
      Vel: Vector2
      IsMoving: bool

      CollisionInfo: CollisionInfo }

let initPlayer x y (playerConfig: PlayerConfig) (spriteConfig: SpriteConfig) =
    let p = Vector2(float32 x, float32 y)

    { SpriteInfo = Sprite.init p spriteConfig
      CharacterState = Small true
      Input = Vector2.Zero
      Carrying =
        [ Entity.initNoCollider Entity.Observer p
          Entity.initNoCollider Entity.Observer p
          Entity.initNoCollider Entity.Timer p
          Entity.initNoCollider Entity.Timer p ]
      Facing = Vector2(1f, 0f)
      Pos = p
      MaxVelocity = playerConfig.SmallMaxVelocity
      Acc = playerConfig.Acc
      Friction = playerConfig.Slow
      Vel = Vector2.Zero
      IsMoving = false
      CollisionInfo =
        { Half = playerConfig.AABBConfig.Half
          Offset = playerConfig.AABBConfig.Offset } }

type PhysicsInfo =
    { Time: int64
      PossibleObstacles: AABB seq }

type PlayerMessage =
    | Input of dir: Vector2
    | TransformCharacter
    | PlayerPhysicsTick of info: PhysicsInfo
    | SpriteMessage of Sprite.Message
    | CarryingMessage of Sprite.Message

let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let collider (pos: Vector2) (collisionInfo: CollisionInfo) : AABB =
    { Pos = pos + collisionInfo.Offset
      Half = collisionInfo.Half }

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

let inputAffectsVelocityAssertions (input: Vector2) (oldVel: Vector2) (newVel: Vector2) : bool =
    if input = Vector2.Zero then
        newVel.Length() <= oldVel.Length() + AcceptableError
    else
        Vector2.Dot(input, newVel) >= Vector2.Dot(input, newVel) - AcceptableError

let updateCarryingPositions (carrying: Entity.Model list) (pos: Vector2) (charState: CharacterState) =
    Cmd.ofMsg (CarryingMessage(Sprite.Message.SetPos pos))


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

    let preCollisionPos = model.Pos + (vel * dt) * pixelsPerMeter

    //collide with walls
    let pos =
        collide preCollisionPos model.Pos model.CollisionInfo info.PossibleObstacles

    let facing = Vector2((float32 << sign) vel.X, (float32 << sign) vel.Y)

    //halting will not change the direction faced :)
    let facing =
        if facing = Vector2.Zero then
            model.Facing
        else
            Vector2.Normalize(facing)

    { model with
        Facing = facing
        Vel = vel
        Pos = pos
        IsMoving = velLength > 0f }

let animations newModel oldModel =
    let directionCommands =
        if newModel.Facing.X <> oldModel.Facing.X then
            [ Cmd.ofMsg (SpriteMessage(Sprite.SetDirection(newModel.Facing.X < 0f))) ]
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

    let carryCommand =
        updateCarryingPositions newModel.Carrying newModel.Pos newModel.CharacterState

    Cmd.batch [ setPosMsg; carryCommand; yield! animationCommands; yield! directionCommands ]

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

let renderAABB (aabb: AABB) (cameraPos: Vector2) =
    image
        "tile"
        Color.Red
        (int (aabb.Half.X * 2f), int (aabb.Half.Y * 2f))
        (int (aabb.Pos.X - aabb.Half.X - cameraPos.X), int (aabb.Pos.Y - aabb.Half.Y - cameraPos.Y))

let renderCarrying (carrying: Entity.Model list) (cameraPos: Vector2) (charState: CharacterState) =
    let offsetStart =
        match charState with
        | Small true -> Vector2(0f, 40f)
        | Small false -> Vector2(0f, 70f)
        | _ -> Vector2(0f, 55f)

    carrying
    |> List.indexed
    |> List.collect (fun (i, c) ->
        let offSetPos = cameraPos + offsetStart + (Vector2(0f, 25f) * (float32 i))
        Sprite.view c.Sprite offSetPos (fun f -> ()))

[<Struct>]
type Tile =
    { FloorType: FloorType
      Collider: AABB option
      Entity: Entity.Model option }

type Model =
    { Tiles: Tile[]
      TileWidth: int

      ChunkBlockLength: int

      //player and camera
      Player: PlayerModel
      CameraPos: Vector2 }


let updateCameraPos (playerPos: Vector2) (oldCamPos: Vector2) : Vector2 =
    let diff = Vector2.Subtract(playerPos, oldCamPos)
    let halfDiff = Vector2.Multiply(diff, 0.25f)

    if halfDiff.LengthSquared() < 0.5f then
        playerPos
    else
        oldCamPos + halfDiff

let halfScreenOffset (camPos: Vector2) : Vector2 =
    Vector2.Subtract(camPos, Vector2(800f, 450f))

let getCollidables (blocks: Tile[]) : AABB seq =
    blocks
    |> Seq.choose (fun block ->
        match block.Collider with
        | Some collider -> Some collider
        | _ ->
            match block.Entity with
            | Some { Collider = collider } -> collider
            | _ -> None)

let getTileAtPos (pos: Vector2) (tiles: Tile[]) : (Tile * int) option =
    let (x, y) = posToCoords pos

    if
        x >= worldConfig.WorldTileLength
        || x < 0
        || y >= worldConfig.WorldTileLength
        || y < 0
    then
        None
    else
        let index = y * worldConfig.WorldTileLength + x
        Some(tiles[index], index)

let init (worldConfig: WorldConfig) =
    let tileHalf = float32 (worldConfig.TileWidth / 2)
    let half = Vector2(tileHalf)

    let createCollidableTile t xx yy =
        { FloorType = t
          Entity = None
          Collider = Some(createColliderFromCoords xx yy half) }

    let createNonCollidableTile t =
        { FloorType = t
          Collider = None
          Entity = None }

    let createTimerOnGrass (coords: Vector2) =
        let pos = coordsToPos coords.X coords.Y half

        { FloorType = FloorType.Grass
          Collider = None
          Entity = Some(Entity.init Entity.Timer pos) }

    let createObserverOnGrass (coords: Vector2) =
        let pos = coordsToPos coords.X coords.Y half

        { FloorType = FloorType.Grass
          Collider = None
          Entity = Some(Entity.init Entity.Observer pos) }

    let blocks =
        [| for yy in 0 .. (worldConfig.WorldTileLength - 1) do
               for xx in 0 .. (worldConfig.WorldTileLength - 1) do
                   let grassTile = createNonCollidableTile FloorType.Grass

                   match xx, yy with
                   | 0, 0 -> createNonCollidableTile FloorType.Grass
                   | 2, 2 -> createTimerOnGrass (Vector2(2f))
                   | 3, 3 -> createObserverOnGrass (Vector2(3f))
                   | 5, 5 -> createCollidableTile FloorType.Empty 5f 5f
                   | 5, 6 -> grassTile // 5f 6f
                   | 7, 9 -> grassTile // 7f 9f
                   | 8, 9 -> grassTile // 8f 9f
                   | 6, 9 -> grassTile // 6f 9f
                   | 7, 8 -> grassTile // 7f 8f
                   | x, y -> grassTile |]

    { Tiles = blocks
      ChunkBlockLength = worldConfig.WorldTileLength
      TileWidth = worldConfig.TileWidth
      Player = initPlayer 0 0 playerConfig charSprite
      CameraPos = Vector2(0f, -0f) }


// UPDATE
type Message =
    | PlayerMessage of PlayerMessage
    | PickUpEntity
    | PlaceEntity
    | PhysicsTick of time: int64


let updatePlayer (message: PlayerMessage) (worldModel: Model) =
    let model = worldModel.Player

    match message with
    | Input direction -> { model with Input = direction }, Cmd.none
    | PlayerPhysicsTick info ->
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

                let modl =
                    { model with
                        CharacterState = newState
                        MaxVelocity = maxVelocity }

                modl, (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (walkAni, 80, modl.IsMoving)
            | Sprite.None -> model, Cmd.none

        { model with SpriteInfo = newSprite }, cmd
    | CarryingMessage sm ->
        let newCarrying =
            model.Carrying
            |> List.mapi (fun i carry ->
                let (newSprite, _) = Sprite.update sm carry.Sprite
                { carry with Sprite = newSprite })

        { model with Carrying = newCarrying }, Cmd.none
    | TransformCharacter ->
        let (newState, transformAnimation) = transformStart model.CharacterState

        { model with CharacterState = newState },
        (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (transformAnimation, 80, true)


let update (message: Message) (model: Model) : Model * Cmd<Message> =
    let player = model.Player
    let targetOffset = 60f

    match message with
    | PlayerMessage playerMsg ->
        let (newPlayerModel, playerCommand) = updatePlayer playerMsg model
        { model with Player = newPlayerModel }, Cmd.map PlayerMessage playerCommand
    | PickUpEntity ->
        match player.CharacterState with
        | Small _ ->
            let tileAndIndex = getTileAtPos (player.Pos + (targetOffset * player.Facing)) model.Tiles // need the concept of 'facing' to add an offset here :'(

            match tileAndIndex with
            | Some({ Entity = Some entity } as tile, i) ->
                model.Tiles[i] <- { tile with Entity = None } // TODO: no mutation
                { model with Player = { player with Carrying = entity :: player.Carrying } }, Cmd.none
            | _ -> model, Cmd.none
        | _ -> model, Cmd.none
    | PlaceEntity ->
        match player.CharacterState with
        | Small _ ->
            let target = player.Pos + (targetOffset * player.Facing) // might have to build this into player??
            let tileAndIndex = getTileAtPos target model.Tiles // need the concept of 'facing' to add an offset here :'(

            match tileAndIndex with
            | Some({ Entity = None } as tile, i) ->
                match player.Carrying with
                | entity :: rest ->
                    let rounded = posRounded target worldConfig

                    model.Tiles[i] <- { tile with Entity = Some(Entity.init entity.Type rounded) } //TODO: no mutation
                    { model with Player = { player with Carrying = rest } }, Cmd.none
                | _ -> model, Cmd.none
            | _ -> model, Cmd.none
        | _ -> model, Cmd.none
    | PhysicsTick time ->
        //TODO: get a list of things the player could interact with
        let (info: PhysicsInfo) =
            { Time = time
              PossibleObstacles = getCollidables model.Tiles }

        let player, playerMsg = updatePlayer (PlayerPhysicsTick info) model
        let newCameraPos = updateCameraPos player.Pos model.CameraPos

        { model with
            CameraPos = newCameraPos
            Player = player },
        Cmd.map PlayerMessage playerMsg


// VIEW
let renderWorld (model: Model) =
    let blockWidth = model.TileWidth
    let empty = "tile"
    let grass = "grass"

    let sourceRect = rect 0 0 blockWidth blockWidth

    let cameraOffset = -(halfScreenOffset model.CameraPos)

    seq {
        for i in 0 .. (model.Tiles.Length - 1) do
            let block = model.Tiles[i]

            let texture =
                match block.FloorType with
                | FloorType.Grass -> grass
                | FloorType.Empty -> empty

            let startX = 0
            let startY = 0

            let xBlockOffSet = (i % model.ChunkBlockLength) * blockWidth
            let yBlockOffSet = (i / model.ChunkBlockLength) * blockWidth

            let actualX = startX + xBlockOffSet + int (cameraOffset.X)
            let actualY = startY + yBlockOffSet + int (cameraOffset.Y)

            let floor =
                image texture Color.White (sourceRect.Width, sourceRect.Height) (actualX, actualY)

            let entity =
                block.Entity
                |> Option.map (fun (entity: Entity.Model) -> Sprite.view entity.Sprite -cameraOffset (fun f -> ()))

            let debug =
                block.Collider
                |> Option.map (fun (b: AABB) ->
                    image
                        empty
                        Color.Red
                        (int (b.Half.X * 2f), int (b.Half.Y * 2f))
                        (int (b.Pos.X - b.Half.X + cameraOffset.X), int (b.Pos.Y - b.Half.Y + cameraOffset.Y)))
                |> Option.toList

            let entityDebug =
                block.Entity
                |> Option.bind (fun e -> e.Collider)
                |> Option.map (fun (b: AABB) ->
                    image
                        empty
                        Color.Red
                        (int (b.Half.X * 2f), int (b.Half.Y * 2f))
                        (int (b.Pos.X - b.Half.X + cameraOffset.X), int (b.Pos.Y - b.Half.Y + cameraOffset.Y)))
                |> Option.toList

            match entity with
            | Some s ->
                yield floor
                yield! s
            //yield! debug
            //yield! entityDebug
            | None -> yield floor
    }

let viewPlayer model (cameraPos: Vector2) (dispatch: PlayerMessage -> unit) =
    [
      //render
      yield! Sprite.view model.SpriteInfo cameraPos (SpriteMessage >> dispatch)
      yield! renderCarrying model.Carrying cameraPos model.CharacterState

      //debug
      yield debugText $"X:{model.Pos.X} \nY:{model.Pos.Y}" (10, 200)
      //yield renderAABB (collider model.Pos model.CollisionInfo) cameraPos

      //IO
      yield directions Keys.Up Keys.Down Keys.Left Keys.Right (fun f -> dispatch (Input f))
      yield onkeydown Keys.Space (fun _ -> dispatch (TransformCharacter)) ]

let view model (dispatch: Message -> unit) =
    [ yield onupdate (fun input -> dispatch (PhysicsTick input.totalGameTime))

      yield! renderWorld model
      yield onkeydown Keys.Z (fun _ -> dispatch (PickUpEntity))
      yield onkeydown Keys.X (fun _ -> dispatch (PlaceEntity))
      yield! viewPlayer model.Player (halfScreenOffset model.CameraPos) (PlayerMessage >> dispatch) ]
