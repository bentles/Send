﻿module World

open Xelmish.Model
open Xelmish.Viewables
open Microsoft.Xna.Framework
open GameConfig
open PlayerConfig
open Elmish
open FsToolkit.ErrorHandling
open Utility
open Level
open Entity
open FSharpx.Collections
open Microsoft.Xna.Framework.Graphics
open Prelude

[<Struct>]
type SongState =
    | PlaySong of song: string
    | PlayingSong of playing: string
    | Stopped

type Model =
    { Tiles: Tiles
      Song: SongState
      LevelText: string

      Size: Coords

      Dt: float32
      Slow: bool
      TimeElapsed: int64

      //player and camera
      PlayerAction: PlayerWorldInteraction
      Player: Player.Model
      PlayerTarget: struct (Tile * int) voption
      PlayerFeet: int voption
      CameraPos: Vector2 }

let halfScreenOffset (camPos: Vector2) : Vector2 =
    Vector2.Subtract(camPos, Vector2(800f, 450f))

let getCollidables (tiles: Tile seq) : AABB seq =
    seq {
        for tile in tiles do
            match tile.Collider with
            | ValueSome collider -> collider
            | _ ->
                match tile.Entity with
                | ValueSome { Collider = ValueSome collider } -> collider
                | _ -> ()
    }

let getIndexAtPos (pos: Vector2) (size: Coords): int voption =
    let coords = offsetVectorToCoords pos
    coordsToIndex coords size

let getTileAtPos (pos: Vector2) (size: Coords) (tiles: Tiles) : struct (Tile * int) voption =
    let index = getIndexAtPos pos size
    index |> ValueOption.map (fun index -> PersistentVector.nth index tiles, index)

let init time =
    let level = levelSandBox time

    { Tiles = level.Tiles
      Song = PlaySong "pewpew"
      Size = level.Size
      LevelText = level.LevelText
      PlayerAction = NoAction
      Player = Player.init level.PlayerStartsAtPos level.PlayerStartsCarrying playerConfig charSprite time
      Slow = false
      Dt = 0f
      PlayerTarget = ValueNone
      PlayerFeet = ValueNone
      TimeElapsed = 0
      CameraPos = Vector2(0f, -0f) }

// UPDATE
type Message =
    | PlayerMessage of Player.Message
    | PickUpEntity
    | PlaceEntity
    | Interact
    | SongStarted of string
    | PhysicsTick of time: int64 * slow: bool

let updateWorldReactive (tiles: Tiles) ((width, height): Coords) : Tiles =
    tiles
    |> PersistentVector.map (fun tile ->
        let maybeEntity =
            voption {
                let! entity = tile.Entity

                let newEntityType =
                    match entity.Type with
                    | Subject subject -> Subject((getSubjectFunc subject.Type) subject)
                    | Observable({ Type = oType } as oData) ->

                        // get what is being is observed if anything
                        let getObserved (tile: Tile) (facing: Facing) =
                            voption {
                                let struct (tx, ty) = facingToCoords (facing)
                                let struct (x, y) = tile.Coords
                                let! targetIndex = (coordsToIndex (x + tx, y + ty) (width, height))
                                let tile = PersistentVector.nth targetIndex tiles
                                let! e = tile.Entity
                                return e.Type
                            }

                        let observerType = getObserverType oType

                        let eType1, eType2 =
                            match observerType with
                            | SingleObserver -> (getObserved tile entity.Facing), ValueNone
                            | DoubleObserver ->
                                (getObserved tile entity.Facing), (getObserved tile (rotateFacing entity.Facing true))

                        Observable(observerFunc oData eType1 eType2)
                    | other -> other

                // if emitting and has an onEmit fun then apply that
                let onEmit =
                    match newEntityType with
                    | EmittingObservable _ ->
                        let pos = (CoordsFToOffsetVector (toCoordsF tile.Coords) half)
                        getOnEmit newEntityType pos
                    | _ -> id

                return onEmit { entity with Type = newEntityType }
            }

        { tile with Entity = maybeEntity })

let updateWorldSprites (totalTime: int64) (tiles: Tiles) : Tiles =
    tiles
    |> PersistentVector.map (fun tile ->
        let entityy =
            voption {
                let! entity = tile.Entity
                let struct (sprite, _) = Sprite.animTick totalTime entity.Sprite
                let r = ({ entity with Sprite = sprite })
                return r
            }

        match entityy with
        | ValueSome(entity) -> { tile with Entity = ValueSome entity }
        | ValueNone -> { tile with Entity = ValueNone })

let updateCameraPos (playerPos: Vector2) (oldCamPos: Vector2) : Vector2 =
    let diff = Vector2.Subtract(playerPos, oldCamPos)
    let halfDiff = Vector2.Multiply(diff, 0.25f)

    if halfDiff.LengthSquared() < 0.5f then
        playerPos
    else
        oldCamPos + halfDiff



let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let pickUpEntity (model: Model) : Model =
    let player = model.Player
    let playerLimit = Player.getPlayerPickupLimit player.CharacterState

    match player.CharacterState with
    | Player.Small _ when player.Carrying.Length + 1 <= playerLimit ->
        option {
            let! (tile, i) = model.PlayerTarget
            let! targetEntity = tile.Entity

            if targetEntity.CanBePickedUp then
                let newTile, pickedUpEntity =
                    match targetEntity with
                    | { Type = NonEmptyObservable(obData, entityType) } as targetEntity ->
                        let newTarget = { targetEntity with Type = Observable(takeOutOfObservable obData) }
                        let fromObserverEntity = Entity.init entityType Vector2.Zero 0 FacingRight true
                        let tile = { tile with Entity = ValueSome newTarget }
                        tile, fromObserverEntity
                    | { Type = Box({ Items = first :: rest; IsOpen = true } as box) } as boxEntity ->
                        let sprite =
                            match rest with
                            | [] ->
                                Sprite.switchAnimation
                                    ({ imageSpriteConfig with Index = 0 }, 0, false)
                                    targetEntity.Sprite
                            | _ -> targetEntity.Sprite

                        let restOfBox =
                            { boxEntity with
                                Type = Box { box with Items = rest }
                                Sprite = sprite }

                        let tile = { tile with Entity = ValueSome restOfBox }
                        let fromBoxEntity = Entity.init first Vector2.Zero 0 FacingRight true
                        tile, fromBoxEntity
                    | _ -> { tile with Entity = ValueNone }, targetEntity

                return
                    { model with
                        Tiles = model.Tiles |> PersistentVector.update i newTile
                        Player = { player with Carrying = pickedUpEntity :: player.Carrying } }
            else
                return! None
        }
        |> Option.defaultValue model
    | _ -> model

let placeEntity (model: Model) : Model =
    let player = model.Player

    match player.CharacterState with
    | Player.Small _ ->
        match player.Carrying with
        | placeEntity :: rest ->
            let tileAndIndex = model.PlayerTarget

            match tileAndIndex with
            | ValueSome({ Entity = ValueNone } as tile, i) ->
                //make a targeting function
                let roundedPos = posRounded player.Target worldConfig
                let facing = player.PlacementFacing

                let entity = Entity.init placeEntity.Type roundedPos model.TimeElapsed facing true
                let sprite = Sprite.startAnimation entity.Sprite
                let entity = { entity with Sprite = sprite }

                let tiles =
                    model.Tiles
                    |> PersistentVector.update i { tile with Entity = ValueSome(entity) }

                { model with
                    Tiles = tiles
                    Player = { player with Carrying = rest } }
            | ValueSome({ Entity = ValueSome({ Type = EmptyObservable(obData) } as targetEntity) } as tile, i) ->
                let newObData = placeIntoObservable obData placeEntity.Type
                let newTarget = ValueSome { targetEntity with Type = Observable newObData }

                let tiles =
                    model.Tiles |> PersistentVector.update i { tile with Entity = newTarget }

                { model with
                    Tiles = tiles
                    Player = { player with Carrying = rest } }
            | ValueSome({ Entity = ValueSome({ Type = Box { Items = contents; IsOpen = true } } as box) } as tile, i) ->
                //can maybe replace this with new type and Entity init?
                let sprite =
                    match contents with
                    | [] -> Sprite.switchAnimation ({ imageSpriteConfig with Index = 2 }, 0, false) box.Sprite
                    | _ -> box.Sprite

                let newBoxType =
                    Box
                        { Items = placeEntity.Type :: contents
                          IsOpen = true }

                let appendedBox =
                    ValueSome(
                        { box with
                            Sprite = sprite
                            Type = newBoxType }
                    )

                let tiles =
                    model.Tiles |> PersistentVector.update i { tile with Entity = appendedBox }

                { model with
                    Tiles = tiles
                    Player = { player with Carrying = rest } }
            | _ -> model
        | _ -> model
    | _ -> model

let changeLevel (model: Model) (levelBuilder: LevelBuilder) : Model =
    let newLevel = levelBuilder model.TimeElapsed

    { model with
        Size = newLevel.Size
        Tiles = newLevel.Tiles
        LevelText = newLevel.LevelText
        Player =
            { model.Player with
                Pos = newLevel.PlayerStartsAtPos
                Carrying = newLevel.PlayerStartsCarrying } }

let interactionEvent (event: Entity.InteractionEvent) (model: Model) : Model =
    match event with
    | GoToLevel l -> changeLevel model (levelLookup l)
    | NoEvent -> model

let update (message: Message) (model: Model) : Model =
    match message with
    | PlayerMessage playerMsg ->
        let (newPlayerModel) = Player.update playerMsg model.Player
        { model with Player = newPlayerModel }
    | SongStarted name -> { model with Song = PlayingSong name }
    | PickUpEntity -> { model with PlayerAction = TryPickup }
    | PlaceEntity -> { model with PlayerAction = TryPlace }
    | Interact ->
        let maybeUpdate =
            voption {
                let! (tile, i) = model.PlayerTarget
                let! entity = tile.Entity
                let newEntity, event = Entity.interact entity

                let tiles =
                    model.Tiles
                    |> PersistentVector.update i { tile with Entity = ValueSome newEntity }

                return interactionEvent event { model with Tiles = tiles }
            }

        match maybeUpdate with
        | ValueNone -> model
        | ValueSome updatedModel -> updatedModel

    | PhysicsTick(time, slow) ->
        let wasCarrying = model.Player.Carrying.Length

        let model =
            match model.PlayerAction with
            | TryPickup -> pickUpEntity model
            | TryPlace -> placeEntity model
            | NoAction -> model

        let isCarrying = model.Player.Carrying.Length

        let dt = (float32 (time - lastTick)) / 1000f
        lastTick <- time

        let (info: PhysicsInfo) =
            { Time = time
              Dt = dt
              PossibleObstacles = getCollidables model.Tiles }

        let player = Player.tick info model.Player
        let maybeTarget = getTileAtPos player.Target model.Size model.Tiles
        let maybeFeetIndex = getIndexAtPos player.Pos model.Size

        let tiles = updateWorldReactive model.Tiles model.Size
        //TODO: move this outside if update ticks < 60ps
        let tiles = updateWorldSprites time tiles

        let newCameraPos = updateCameraPos player.Pos model.CameraPos

        { model with
            Dt = dt
            Slow = slow
            TimeElapsed = time
            Tiles = tiles
            CameraPos = newCameraPos
            Player = { player with CarryingDelta = isCarrying - wasCarrying }
            PlayerTarget = maybeTarget
            PlayerFeet = maybeFeetIndex
            PlayerAction = NoAction }

// VIEW

let viewEmitting
    (entityType: EntityType)
    (ticksSinceLast: int)
    pos
    (spriteBatch: SpriteBatch)
    (texture: Graphics.Texture2D)
    : unit =
    let imageInfo = getEmitImage entityType
    let struct (width, height) = (imageInfo.SpriteSize)
    let timeToGone = 10

    if ticksSinceLast < timeToGone then
        let alpha = int ((float32 (15 - ticksSinceLast) / (float32 timeToGone)) * 220f)
        let dwidth, dheight = (int ((float width) / 1.5), int ((float height) / 1.5))
        let x, y = pos

        spriteBatch.Draw(
            texture,
            Rectangle(x + 20, y - 5, dwidth, dheight),
            Rectangle(0, 0, width, height),
            (Color.FromNonPremultiplied(255, 255, 255, alpha))
        )

let viewObserverItem
    (entityType: EntityType)
    (ticksSinceLast: int)
    pos
    (spriteBatch: SpriteBatch)
    (texture: Graphics.Texture2D)
    : unit =
    let imageInfo = getEmitImage entityType
    let struct (width, height) = (imageInfo.SpriteSize)

    if ticksSinceLast < 20 then
        let alpha = int ((float32 (30 - ticksSinceLast) / 20f) * 220f)
        let dwidth, dheight = (int ((float width) / 1.5), int ((float height) / 1.5))
        let x, y = pos

        spriteBatch.Draw(
            texture,
            Rectangle(x + 20, y + 18, dwidth, dheight),
            Rectangle(0, 0, width, height),
            (Color.FromNonPremultiplied(255, 255, 255, alpha))
        )

let blockWidth = worldConfig.TileWidth
let empty = "tile"
let grass = "grass"
let wall = "wall"
let leftWall = "leftWall"
let rightWall = "rightWall"
let topWall = "topWall"
let bottomWall = "bottomWall"

let viewWorld (model: Model) loadedAssets (spriteBatch: SpriteBatch) =
    let sourceRect = rect 0 0 blockWidth blockWidth
    let cameraOffset = -(halfScreenOffset model.CameraPos)

    spriteBatch.DrawString(loadedAssets.fonts["defaultFont"], model.LevelText, Vector2(0f, 0f), Color.White)

    model.Tiles
    |> PersistentVector.toSeq
    |> Seq.iteri (fun i tile ->

        let texture =
            match tile.FloorType with
            | FloorType.Grass -> grass
            | FloorType.Empty -> empty
            | FloorType.Wall -> wall
            | FloorType.LeftWall -> leftWall
            | FloorType.RightWall -> rightWall
            | FloorType.TopWall -> topWall
            | FloorType.BottomWall -> bottomWall

        let startX = 0
        let startY = 0
        let struct (width, _) = model.Size

        let xBlockOffSet = (i % width) * blockWidth
        let yBlockOffSet = (i / width) * blockWidth

        let actualX = startX + xBlockOffSet + int (cameraOffset.X)
        let actualY = startY + yBlockOffSet + int (cameraOffset.Y)

        //floor
        spriteBatch.Draw(
            loadedAssets.textures[texture],
            Rectangle(actualX, actualY, sourceRect.Width, sourceRect.Height),
            Color.White
        )

        let alpha = 0.5f
        //target
        let maybeTargetColor =
            voption {
                let! (tile, ind) = model.PlayerTarget
                let! target = if i = ind then ValueSome tile else ValueNone
                let illegal = ValueOption.isSome target.Collider || ValueOption.isSome target.Entity

                return
                    if illegal then
                        (Color.Orange * alpha)
                    else
                        (Color.Green * alpha)
            }

        let struct (texture, effect) =
            match model.Player.PlacementFacing with
            | FacingUp -> "facingUp", SpriteEffects.None
            | FacingRight -> "facingRight", SpriteEffects.None
            | FacingDown -> "facingUp", SpriteEffects.FlipVertically
            | FacingLeft -> "facingRight", SpriteEffects.FlipHorizontally


        match maybeTargetColor with
        | ValueSome color ->
            spriteBatch.Draw(
                loadedAssets.textures[texture],
                Rectangle(actualX, actualY, sourceRect.Width, sourceRect.Height),
                System.Nullable<Rectangle>(),
                color,
                0f,
                Vector2.Zero,
                effect,
                0f
            )
        | ValueNone -> ()

        match model.PlayerFeet with
        | ValueSome index when index = i ->
            spriteBatch.Draw(
                    loadedAssets.textures["feet"],
                    Rectangle(actualX, actualY, sourceRect.Width, sourceRect.Height),
                    System.Nullable<Rectangle>(),
                    Color.Green * alpha,
                    0f,
                    Vector2.Zero,
                    effect,
                    0f
                )
        | _ -> ()

        match tile.Entity with
        | ValueSome entity ->
            Sprite.drawSprite entity.Sprite -cameraOffset loadedAssets spriteBatch

            match entity.Type with
            | EmittingObservable(_, _) -> () // loadedAssets.sounds[ "click" ].Play(1f, 0.0f, 0.0f)  |> ignore
            | _ -> ()

            match entity.Type with
            | RenderEmittingObservable(etype, t) ->
                viewEmitting
                    etype
                    t
                    (actualX, actualY)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage etype).TextureName]
            | _ -> ()

            match entity.Type, maybeTargetColor with
            | EmptyObservable(_), ValueSome _ ->
                viewObserverItem
                    Unit
                    1
                    (actualX, actualY)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage Unit).TextureName]
            | NonEmptyObservable(_, eType), ValueSome _ ->
                viewObserverItem
                    eType
                    1
                    (actualX, actualY)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage eType).TextureName]
            | _ -> ()

        | ValueNone -> ())


let draw model (dispatch: Message -> unit) loadedAssets _inputs spriteBatch =
    match model.Song with
    | PlaySong songName ->
        Media.MediaPlayer.Volume <- 0.4f
        //Media.MediaPlayer.Play(loadedAssets.music[songName])
        Media.MediaPlayer.IsRepeating <- true
        dispatch (SongStarted songName)
    | Stopped -> Media.MediaPlayer.Stop()
    | _ -> ()

    viewWorld model loadedAssets spriteBatch
    Player.viewPlayer model.Player (halfScreenOffset model.CameraPos) loadedAssets spriteBatch

let inputs (inputs: Inputs) (dispatch: Message -> unit) =
    if KeyBoard.iskeydown Keys.X inputs then
        (dispatch (PickUpEntity))

    if KeyBoard.iskeydown Keys.C inputs then
        (dispatch (PlaceEntity))

    if KeyBoard.iskeydown Keys.Z inputs then
        (dispatch (Interact))

    Player.inputs inputs (PlayerMessage >> dispatch)
    dispatch (PhysicsTick(inputs.totalGameTime, inputs.gameTime.IsRunningSlowly))
