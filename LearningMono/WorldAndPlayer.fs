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

type PlayerTarget = voption<struct (Tile * int)>

type Model =
    { Tiles: Tiles
      Song: SongState
      LevelText: string
      Level: int

      Size: Coords

      Dt: float32
      Slow: bool
      TimeElapsed: int64
      TicksElapsed: int64

      //player and camera
      PlayerAction: PlayerWorldInteraction
      Player: Player.Model
      PlayerTarget: PlayerTarget
      PlayerFeet: int voption
      CameraPos: Vector2 }

let getWidth (model: Model) : int32 =
    let struct (width, _) = model.Size
    width

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

let getTileAtIndex index (tiles: Tiles) = PersistentVector.nth index tiles

let getTileAtPos (pos: Vector2) (size: Coords) (tiles: Tiles) : struct (Tile * int) voption =
    let index = getIndexAtPos pos size
    index |> ValueOption.map (fun index -> getTileAtIndex index tiles, index)

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
      TicksElapsed = 0
      CameraPos = Vector2(0f, -0f) }

// UPDATE
type Message =
    | PlayerMessage of Player.Message
    | PickUpEntity
    | PushEntity
    | PlaceEntity
    | MultiPlaceEntity
    | EndMultiPlace
    | Interact
    | SongStarted of string
    | PhysicsTick of time: int64 * slow: bool

let updateWorldReactive (tiles: Tiles) (ticksElapsed: int64) ((width, height): Coords) : Tiles =
    //only do reactive updates every TicksPerReactiveUpdate ticks
    if (ticksElapsed % WorldConfig.TicksPerReactiveUpdate) = 0 then
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
                                    (getObserved tile entity.Facing),
                                    (getObserved tile (rotateFacing entity.Facing true))

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
    else
        tiles

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

type PickupEntityFn = Entity.Model -> int32 -> Tile -> Model -> Model

let pickUp (targetEntity: Entity.Model) (i: int) (tile: Tile) (model: Model) : Model =
    let player = model.Player

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


    { model with
        Tiles = model.Tiles |> PersistentVector.update i newTile
        Player =
            { player with
                Carrying = pickedUpEntity :: player.Carrying } }

let pickUpEntityImpl (pickupFn: PickupEntityFn) (model: Model) (target: PlayerTarget) : Model =
    let player = model.Player
    let playerLimit = Player.getPlayerPickupLimit player.CharacterState

    match player.CharacterState with
    | Player.Small _ when player.Carrying.Length + 1 <= playerLimit ->
        option {
            let! (tile, i) = target
            let! targetEntity = tile.Entity

            if targetEntity.CanBePickedUp then
                return pickupFn targetEntity i tile model
            else
                return! None
        }
        |> Option.defaultValue model
    | _ -> model

let pickUpEntity (model: Model) : Model =
    pickUpEntityImpl pickUp model model.PlayerTarget

type PlaceDownFn = Model -> Tiles -> List<Entity.Model> -> int64 -> Coords -> Model

let placeDown (model: Model) (tiles: Tiles) (rest: List<Entity.Model>) (time: int64) (coords: Coords) : Model =
    let curMulti = model.Player.MultiPlace

    let newMulti: Player.MultiPlace =
        match curMulti with
        | ValueSome multi ->
            { multi with
                LastTime = time
                Coords = coords }
        | ValueNone ->
            let facing = vectorToFacingDefault model.Player.Facing

            { LastTime = time
              Coords = coords
              Facing = facing }

    { model with
        Tiles = tiles
        Player =
            { model.Player with
                Carrying = rest
                MultiPlace = ValueSome newMulti } }

let placeEntityAtImpl (placeFn: PlaceDownFn) (coords: Coords) (tile: Tile) (i: int) (model: Model) time : Model =
    let player = model.Player

    match player with
    | Player.PlayerCanPlace(toPlace, rest) ->
        match tile with
        | { Entity = ValueNone
            Collider = ValueNone } ->
            match model.PlayerFeet with
            | ValueSome feet when feet <> i ->
                let entity =
                    Entity.init toPlace.Type (coordsToPos coords) model.TimeElapsed player.PlacementFacing true

                // can't place if the block will intersect with the player
                let col = entity.Collider |> ValueOption.defaultValue Collision.emptyCollider

                voption {
                    let! _ = Collision.noIntersectionAABB (Collision.playerCollider player.Pos) col
                    let tiles = updateTilesWithEntity model.Tiles i tile entity
                    return placeFn model tiles rest time coords
                }
                |> ValueOption.defaultValue model
            | _ -> model
        | { Entity = ValueSome({ Type = CanPlaceIntoEntity toPlace.Type (newEntity) } as targetEntity) } ->
            let newTarg = Entity.updateSprite { targetEntity with Type = newEntity }
            let tiles = updateTilesWithEntity model.Tiles i tile newTarg
            placeFn model tiles rest time coords
        | _ -> model
    | _ -> model


let placeEntityAt (coords: Coords) (tile: Tile) (i: int) (model: Model) time : Model =
    placeEntityAtImpl placeDown coords tile i model time

let placeEntity (model: Model) time =
    let tileAndIndex = model.PlayerTarget

    match tileAndIndex with
    | ValueSome(tile, i) ->
        let width = getWidth model
        let coords = indexToCoords i width
        placeEntityAt coords tile i model time
    | ValueNone -> model

let pushEntity model time =
    let facing = vectorToFacingDefault model.Player.Facing
    let width = getWidth model

    voption {
        let! (_, i) = model.PlayerTarget
        let coords = indexToCoords i width
        let nextCoords = addFacing coords facing
        let! nextI = coordsToIndex nextCoords model.Size
        let nextTile = getTileAtIndex nextI model.Tiles
        let modelAfterPick = pickUpEntity model //normal pick up

        return
            placeEntityAtImpl
                (fun (model': Model) _ _ (time': int64) (coords': Coords) ->
                    placeEntityAt coords' nextTile nextI modelAfterPick time' //then place 1 block onwards
                )
                nextCoords
                nextTile
                nextI
                modelAfterPick
                time
    }
    |> ValueOption.defaultValue model

let multiPlaceEntity (model: Model) time : Model =
    match model.Player.MultiPlace with
    | ValueNone -> model
    | ValueSome multiPlace when time - multiPlace.LastTime > playerConfig.MultiPlaceDelayMs ->
        let nextCoords = addFacing multiPlace.Coords multiPlace.Facing

        voption {
            let! i = coordsToIndex nextCoords model.Size
            let tile = getTileAtIndex i model.Tiles
            return placeEntityAt nextCoords tile i model time
        }
        |> ValueOption.defaultValue model
    | ValueSome _ -> model

let endMultiPlace (model: Model) =
    { model with
        Player = Player.endMultiPlace model.Player }

let orientEntity (model: Model) (facing: Facing) =
    let player = model.Player

    match player.CharacterState with
    | Player.Small _ ->
        let tileAndIndex = model.PlayerTarget

        match tileAndIndex with
        | ValueSome({ Entity = ValueSome({ CanBePickedUp = true } as entityData) } as tile, i) ->
            let entity = Entity.updateSprite { entityData with Facing = facing }
            let tiles = updateTilesWithEntity model.Tiles i tile entity
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
    | PushEntity -> { model with PlayerAction = TryPush }
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
            | TryPush -> pushEntity model time
            | TryMultiPlace true -> multiPlaceEntity model time
            | TryMultiPlace false -> endMultiPlace model
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

        let tiles = updateWorldReactive model.Tiles model.TicksElapsed model.Size

        let goToNextLevel =
            tiles
            |> PersistentVector.fold
                (fun curr next ->
                    curr
                    || match next with
                       | { Entity = ValueSome { Type = Unit } } -> true
                       | _ -> false)
                false

        let tiles = updateWorldSprites time tiles
        let newCameraPos = updateCameraPos player.Pos model.CameraPos

        let model =
            { model with
                Dt = dt
                Slow = slow
                TimeElapsed = time
                TicksElapsed = model.TicksElapsed + 1L
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
    let timeToGone = 30

    if ticksSinceLast < timeToGone then
        let alpha = int ((float32 (30 - ticksSinceLast) / (float32 timeToGone)) * 220f)
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
            DepthConfig.Emitting
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
            DepthConfig.Emitting
        )

let blockWidth = WorldConfig.TileWidth
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
        let width = getWidth model

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
                DepthConfig.Target
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
                DepthConfig.Target
            )
        | _ -> ()

        match tile.Entity with
        | ValueSome entity ->
            let depth =
                match entity.Collider with
                | ValueSome coll -> (coll.Pos.Y * DepthConfig.DepthFactor)
                | ValueNone -> 0f

            Sprite.viewSprite
                entity.Sprite
                -cameraOffset
                loadedAssets
                spriteBatch
                (depth + DepthConfig.Entities_And_Player)

            // match entity.Type with
            // | EmittingObservable(_, _) -> loadedAssets.sounds["click"].Play(0.05f, 0.0f, 0.0f) |> ignore
            // | _ -> ()

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

    if Keyboard.iskeydown Keys.S inputs then
        (dispatch (PushEntity))

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
