module World

open Xelmish.Model
open Microsoft.Xna.Framework
open GameConfig
open PlayerConfig
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
      Level: int

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

let getIndexAtPos (pos: Vector2) (size: Coords) : int voption =
    let coords = offsetVectorToCoords pos
    coordsToIndex coords size

let getTileAtPos (pos: Vector2) (size: Coords) (tiles: Tiles) : struct (Tile * int) voption =
    let index = getIndexAtPos pos size
    index |> ValueOption.map (fun index -> PersistentVector.nth index tiles, index)

let init time =
    let levelIndex = 0 //Levels.levels.Length - 1
    let level = Levels.levels[levelIndex]time

    { Tiles = level.Tiles
      Song = PlaySong "pewpew"
      Size = level.Size
      LevelText = level.LevelText
      Level = levelIndex
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
    | MultiPlaceEntity
    | EndMultiPlace
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
                    | { Type = CanPickOutOfEntity(eData, entityType) } as targetEntity ->
                        let newTarget = Entity.updateSprite { targetEntity with Type = eData }
                        let fromObserverEntity = Entity.init entityType Vector2.Zero 0 FacingRight true

                        let tile =
                            { tile with
                                Entity = ValueSome newTarget }

                        tile, fromObserverEntity
                    | _ -> { tile with Entity = ValueNone }, targetEntity

                return
                    { model with
                        Tiles = model.Tiles |> PersistentVector.update i newTile
                        Player =
                            { player with
                                Carrying = pickedUpEntity :: player.Carrying } }
            else
                return! None
        }
        |> Option.defaultValue model
    | _ -> model

let placeEntityAt (distance: int) (model: Model) time : Model =
    let playerPlaced (model: Model) tiles rest time i : Model =
        let struct (width, _) = model.Size

        let multiPlace: voption<Player.MultiPlace> =
            ValueSome
                { LastTime = time
                  Coords = (indexToCoords i width)
                  Distance = distance
                }

        { model with
            Tiles = tiles
            Player =
                { model.Player with
                    Carrying = rest
                    MultiPlace = multiPlace } }

    let player = model.Player

    match player.CharacterState, player.Carrying with
    | Player.Small _, placeEntity :: rest ->
        let tileAndIndex = model.PlayerTarget
        let feetIndex = model.PlayerFeet

        match tileAndIndex with
        | ValueSome({ Entity = ValueNone
                      Collider = ValueNone } as tile,
                    i) ->

            match feetIndex with
            | ValueSome feet when feet <> i ->
                let roundedPos = posRounded player.Target

                let entity =
                    Entity.init placeEntity.Type roundedPos model.TimeElapsed player.PlacementFacing true

                // can't place if the block will intersect with the player
                voption {
                    let! col = entity.Collider
                    let! _ = Collision.noIntersectionAABB (Collision.playerCollider player.Pos) col
                    let newTile = { tile with Entity = ValueSome entity }
                    let tiles = model.Tiles |> PersistentVector.update i newTile
                    return playerPlaced model tiles rest time i
                }
                |> ValueOption.defaultValue model
            | _ -> model
        | ValueSome({ Entity = ValueSome({ Type = CanPlaceIntoEntity placeEntity.Type (newEntity) } as targetEntity) } as tile,
                    i) ->
            let newTarg = Entity.updateSprite { targetEntity with Type = newEntity }
            let newTile = { tile with Entity = ValueSome newTarg }
            let tiles = model.Tiles |> PersistentVector.update i newTile
            playerPlaced model tiles rest time i
        | _ -> model
    | _ -> model

let placeEntity = placeEntityAt 0

let multiPlaceEntity (model: Model) time : Model =
    match model.Player.MultiPlace with
    | ValueNone -> model
    | ValueSome multiPlace when time - multiPlace.LastTime > playerConfig.MultiPlaceDelayMs ->
        let model = placeEntityAt (multiPlace.Distance + 1) model time 
        let player = model.Player
        let player =
            { player with
                MultiPlace =
                    ValueSome
                        { multiPlace with
                            LastTime = time } }

        { model with Player = player }
    | ValueSome _ -> model


let orientEntity (model: Model) (facing: Facing) =
    let player = model.Player

    match player.CharacterState with
    | Player.Small _ ->
        let tileAndIndex = model.PlayerTarget

        match tileAndIndex with
        | ValueSome({ Entity = ValueSome({ CanBePickedUp = true } as entityData) } as tile, i) ->
            let entity = Entity.updateSprite { entityData with Facing = facing }

            let tiles =
                model.Tiles |> PersistentVector.update i { tile with Entity = ValueSome entity }

            { model with Tiles = tiles }
        | _ -> model
    | _ -> model

let nextLevel (model: Model) : Model =
    let levelIndex = (model.Level + 1) % Levels.levels.Length
    let newLevel = Levels.levels[levelIndex]model.TimeElapsed

    { model with
        Size = newLevel.Size
        Tiles = newLevel.Tiles
        LevelText = newLevel.LevelText
        Level = levelIndex
        Player =
            { model.Player with
                Pos = newLevel.PlayerStartsAtPos
                Carrying = newLevel.PlayerStartsCarrying } }

let interactionEvent (event: Entity.InteractionEvent) (model: Model) : Model =
    match event with
    | NextLevel -> nextLevel model
    | NoEvent -> model

let update (message: Message) (model: Model) : Model =
    match message with
    | PlayerMessage playerMsg ->
        let (newPlayerModel) = Player.update playerMsg model.Player
        // intercept orientation messages
        let playerAction =
            match playerMsg with
            | Player.Message.Input dir when model.Player.ArrowsControlPlacement ->
                let facing = vectorToFacing (Vector2(float32 dir.X, float32 dir.Y))

                match facing with
                | ValueSome facing -> TryOrient facing
                | _ -> model.PlayerAction
            | _ -> model.PlayerAction

        { model with
            Player = newPlayerModel
            PlayerAction = playerAction }
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
                    |> PersistentVector.update
                        i
                        { tile with
                            Entity = ValueSome newEntity }

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
            | TryPlace -> placeEntity model time
            | TryMultiPlace true -> multiPlaceEntity model time
            | TryMultiPlace false -> model
            | TryOrient facing -> orientEntity model facing
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

        let goToNextLevel =
            tiles
            |> PersistentVector.fold
                (fun curr next ->
                    curr
                    || match next with
                       | { Entity = ValueSome { Type = Unit } } -> true
                       | _ -> false)
                false
        //TODO: move this outside if update ticks < 60ps
        let tiles = updateWorldSprites time tiles

        let newCameraPos = updateCameraPos player.Pos model.CameraPos

        let model =
            { model with
                Dt = dt
                Slow = slow
                TimeElapsed = time
                Tiles = tiles
                CameraPos = newCameraPos
                Player =
                    { player with
                        CarryingDelta = isCarrying - wasCarrying }
                PlayerTarget = maybeTarget
                PlayerFeet = maybeFeetIndex
                PlayerAction = NoAction }

        if goToNextLevel then nextLevel model else model
    | MultiPlaceEntity ->
        { model with
            PlayerAction = TryMultiPlace true }
    | EndMultiPlace ->
        { model with
            PlayerAction = TryMultiPlace false }


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
            Rectangle(x + 25, y - 10, dwidth, dheight),
            Rectangle(0, 0, width, height),
            Color.FromNonPremultiplied(255, 255, 255, alpha),
            0f,
            Vector2.Zero,
            SpriteEffects.None,
            depthConfig.Emitting
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
            (Color.FromNonPremultiplied(255, 255, 255, alpha)),
            0f,
            Vector2.Zero,
            SpriteEffects.None,
            depthConfig.Emitting
        )

let blockWidth = worldConfig.TileWidth
let empty = "tile"
let _void = "void"
let grass = "grass"
let wall = "wall"
let leftWall = "leftWall"
let rightWall = "rightWall"
let topWall = "topWall"
let bottomWall = "bottomWall"
let floor = "floor"


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
            | FloorType.Void -> _void
            | FloorType.Wall -> wall
            | FloorType.LeftWall -> leftWall
            | FloorType.RightWall -> rightWall
            | FloorType.TopWall -> topWall
            | FloorType.BottomWall -> bottomWall
            | FloorType.Floor -> floor

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
                depthConfig.Target
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
                depthConfig.Target
            )
        | _ -> ()

        match tile.Entity with
        | ValueSome entity ->
            let depth =
                match entity.Collider with
                | ValueSome coll -> (coll.Pos.Y * DepthFactor)
                | ValueNone -> 0f

            Sprite.viewSprite
                entity.Sprite
                -cameraOffset
                loadedAssets
                spriteBatch
                (depth + depthConfig.Entities_And_Player)

            match entity.Type with
            | EmittingObservable(_, _) -> loadedAssets.sounds["click"].Play(0.3f, 0.0f, 0.0f) |> ignore
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
            | CanPickOutOfEntity(_, eType), ValueSome _ ->
                viewObserverItem
                    eType
                    1
                    (actualX, actualY)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage eType).TextureName]
            | CanPlaceIntoEntity Unit (_), ValueSome _ ->
                viewObserverItem
                    Unit
                    1
                    (actualX, actualY)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage Unit).TextureName]
            | _ -> ()

        | ValueNone -> ()

        match tile.Entity with
        | ValueSome { Collider = ValueSome collider } ->
            Collision.viewAABB collider (-cameraOffset) loadedAssets spriteBatch
        | _ -> ())


let view model (dispatch: Message -> unit) loadedAssets _inputs spriteBatch =
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
    if Keyboard.iskeydown Keys.X inputs then
        (dispatch (PickUpEntity))

    if Keyboard.iskeydown Keys.C inputs then
        (dispatch (PlaceEntity))

    if Keyboard.whilekeydown Keys.C inputs then
        (dispatch (MultiPlaceEntity))

    if Keyboard.iskeyup Keys.C inputs then
        (dispatch (EndMultiPlace))

    if Keyboard.iskeydown Keys.Z inputs then
        (dispatch (Interact))

    Player.inputs inputs (PlayerMessage >> dispatch)
    dispatch (PhysicsTick(inputs.totalGameTime, inputs.gameTime.IsRunningSlowly))
