module Game

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
open Microsoft.Xna.Framework.Input

type LevelTransition = {
    Message: string
    TimeToShow: float32
}

type Model =
    { World: World
      LevelTransition: voption<LevelTransition>
      Player: Player.Model
      CameraPos: Vector2 }

let getWidth (model: Model) : int32 =
    let struct (width, _) = model.World.Size
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

let calculateTargets (tiles: Tiles) (size: Coords) (player: Player.Model) =
    let maybeTarget = Level.getTileAtPos player.Target size tiles
    let maybeFeetIndex = Level.getIndexAtPos player.Pos size
    struct (maybeTarget, maybeFeetIndex)

let init time =
    let levelIndex = 0 //Levels.levels.Length - 1
    let level = Levels.levels[levelIndex]time

    { World =
        { Tiles = level.Tiles
          Song = PlaySong "pewpew"
          Size = level.Size
          LevelText = level.LevelText
          Level = levelIndex

          Slow = false
          Dt = 0f

          TimeElapsed = 0
          TicksElapsed = 0 }
      LevelTransition = ValueSome { Message = "Test"; TimeToShow = 0.5f }
      Player = Player.init level.PlayerStartsAtPos level.PlayerStartsCarrying playerConfig charSprite time
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


let updateTile (model: Model) (i: int) (tile: Tile) : Model =
    let newTiles = model.World.Tiles |> PersistentVector.update i tile
    let struct (target, feet) = calculateTargets newTiles model.World.Size model.Player

    { model with
        World = { model.World with Tiles = newTiles }
        Player =
            { model.Player with
                TargetedTile = target
                Feet = feet } }

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
    let newTile, pickedUpEntity =
        match targetEntity with
        | { Type = CanPickOutOfEntity(ValueSome eData, entityType) } as targetEntity ->
            let newTarget = Entity.updateSprite { targetEntity with Type = eData }
            let fromObserverEntity = Entity.init entityType Vector2.Zero 0 FacingRight true

            let tile =
                { tile with
                    Entity = ValueSome newTarget }

            tile, fromObserverEntity
        | { Type = CanPickOutOfEntity(ValueNone, entityType) } ->
            let fromObserverEntity = Entity.init entityType Vector2.Zero 0 FacingRight true
            let tile =
                { tile with
                    Entity = ValueNone }

            tile, fromObserverEntity
        | _ -> { tile with Entity = ValueNone }, targetEntity

    let model = updateTile model i newTile

    let newPlayer =
        { model.Player with
            Carrying = pickedUpEntity :: model.Player.Carrying }

    { model with Player = newPlayer }

let pickUpEntityImpl (pickupFn: PickupEntityFn) (model: Model) : Model =
    let player = model.Player

    match player with
    | Player.PlayerCanPickup ->
        voption {
            let! (tile, i) = player.TargetedTile
            let! targetEntity = tile.Entity

            if targetEntity.CanBePickedUp then
                return pickupFn targetEntity i tile model
            else
                return! None
        }
        |> ValueOption.defaultValue model
    | Player.PlayerCantPickup -> model

let pickUpEntity (model: Model) : Model = pickUpEntityImpl pickUp model

type PlaceDownFn = Model -> Tile * int32 -> Coords -> List<Entity.Model> -> Entity.Model -> int64 -> Model

let placeDown: PlaceDownFn =
    fun model (tile, i) coords rest entity time ->
        let curMulti = model.Player.MultiPlace
        let model = updateTile model i { tile with Entity = ValueSome entity }

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
            Player =
                { model.Player with
                    Carrying = rest
                    MultiPlace = ValueSome newMulti } }

let placeEntityAtImpl
    (placeFn: PlaceDownFn)
    (noPlaceFn: Model -> Model)
    (coords: Coords)
    (tile: Tile)
    (i: int)
    (model: Model)
    time
    : Model =
    let player = model.Player

    match player with
    | Player.PlayerCanPlace(toPlace, rest) ->
        match tile with
        | { Entity = ValueNone
            Collider = ValueNone } ->
            match model.Player.Feet with
            | ValueSome feet when feet <> i ->
                let entity =
                    Entity.init toPlace.Type (coordsToPos coords) model.World.TimeElapsed player.PlacementFacing true

                // can't place if the block will intersect with the player
                let col = entity.Collider |> ValueOption.defaultValue Collision.emptyCollider

                let collidedWithPlayer =
                    Collision.intersectAABB (Collision.playerCollider player.Pos) col

                match collidedWithPlayer with
                | ValueSome _ -> noPlaceFn model
                | ValueNone -> placeFn model (tile, i) coords rest entity time
            | _ -> noPlaceFn model
        | { Entity = ValueSome({ Type = CanPlaceIntoEntity toPlace.Type (newEntity) } as targetEntity) } ->
            let newTarg = Entity.updateSprite { targetEntity with Type = newEntity }
            placeFn model (tile, i) coords rest newTarg time
        | _ -> noPlaceFn model
    | Player.PlayerCantPlace -> (noPlaceFn model)

let placeEntityAt (coords: Coords) (tile: Tile) (i: int) (model: Model) time : Model =
    placeEntityAtImpl placeDown id coords tile i model time

let placeEntity (model: Model) time =
    let tileAndIndex = model.Player.TargetedTile

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
        let! (_, i) = model.Player.TargetedTile
        let coords = indexToCoords i width
        let nextCoords = addFacing coords facing
        let! nextI = coordsToIndex nextCoords model.World.Size
        let nextTile = getTileAtIndex nextI model.World.Tiles

        let modelAfterPick = pickUpEntity model //normal pick up

        let hasPickedUp =
            modelAfterPick.Player.Carrying.Length > model.Player.Carrying.Length


        return
            if hasPickedUp then
                placeEntityAtImpl
                    (fun _ _ (coords': Coords) _ _ (time': int64) ->
                        placeEntityAt coords' nextTile nextI modelAfterPick time' //then place 1 block onwards
                    )
                    (fun modl -> placeEntity modl time)
                    nextCoords
                    nextTile
                    nextI
                    modelAfterPick
                    time
            else
                model
    }
    |> ValueOption.defaultValue model

let multiPlaceEntity (model: Model) time : Model =
    match model.Player.MultiPlace with
    | ValueNone -> model
    | ValueSome multiPlace when time - multiPlace.LastTime > playerConfig.MultiPlaceDelayMs ->
        let nextCoords = addFacing multiPlace.Coords multiPlace.Facing

        voption {
            let! i = coordsToIndex nextCoords model.World.Size
            let tile = getTileAtIndex i model.World.Tiles
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
        let tileAndIndex = model.Player.TargetedTile

        match tileAndIndex with
        | ValueSome({ Entity = ValueSome({ CanBePickedUp = true } as entityData) } as tile, i) ->
            let entity = Entity.updateSprite { entityData with Facing = facing }
            updateTile model i { tile with Entity = ValueSome entity }
        | _ -> model
    | _ -> model

let nextLevel (model: Model) : Model =
    let levelIndex = (model.World.Level + 1) % Levels.levels.Length
    let newLevel = Levels.levels[levelIndex]model.World.TimeElapsed

    let newWorld =
        { model.World with
            Size = newLevel.Size
            Tiles = newLevel.Tiles
            LevelText = newLevel.LevelText
            Level = levelIndex }

    let newPlayer =
        { model.Player with
            Pos = newLevel.PlayerStartsAtPos
            Carrying = newLevel.PlayerStartsCarrying }

    { model with
        LevelTransition = ValueSome { Message = "aloha"; TimeToShow = 0.8f }
        World = newWorld
        Player = newPlayer }

let interactionEvent (event: Entity.InteractionEvent) (model: Model) : Model =
    match event with
    | NextLevel -> nextLevel model
    | NoEvent -> model


let update (message: Message) (model: Model) : Model =
    let player = model.Player
    let setPlayerAction = Player.setAction player

    match message with
    | PlayerMessage playerMsg ->
        let newPlayer = Player.update playerMsg player
        // intercept orientation messages
        let playerAction =
            match playerMsg with
            | Player.Message.Input dir when player.ArrowsControlPlacement ->
                let facing = vectorToFacing (Vector2(float32 dir.X, float32 dir.Y))

                match facing with
                | ValueSome facing -> TryOrient facing
                | _ -> player.Action
            | _ -> player.Action

        { model with
            Player = Player.setAction newPlayer playerAction }
    | SongStarted name ->
        { model with
            World =
                { model.World with
                    Song = PlayingSong name } }
    | PickUpEntity ->
        { model with
            Player = setPlayerAction TryPickup }
    | PushEntity ->
        let newPlayer =
            match player.CharacterState with
            | Player.Small false ->
                { player with
                    SpriteInfo = Sprite.switchAnimation (CharAnimations.BigAttack, 50, true) player.SpriteInfo }
            | _ -> player

        { model with
            Player = Player.setAction newPlayer TryPush }

    | PlaceEntity ->
        { model with
            Player = setPlayerAction TryPlace }
    | Interact ->
        let maybeUpdate =
            voption {
                let! (tile, i) = player.TargetedTile
                let! entity = tile.Entity
                let newEntity, event = Entity.interact entity

                let newTile =
                    { tile with
                        Entity = ValueSome newEntity }

                let model = updateTile model i newTile
                return interactionEvent event model
            }

        match maybeUpdate with
        | ValueNone -> model
        | ValueSome updatedModel -> updatedModel

    | PhysicsTick(time, slow) ->
        let dt = (float32 (time - lastTick)) / 1000f
        lastTick <- time

        match model.LevelTransition with
        | ValueSome transition -> 
            let newTime = transition.TimeToShow - dt
            if newTime < 0f then {model with LevelTransition = ValueNone } else {model with LevelTransition = ValueSome { transition with TimeToShow = newTime }}
        | ValueNone ->
            let wasCarrying = player.Carrying.Length

            let model =
                match model.Player.Action with
                | TryPickup -> pickUpEntity model
                | TryPlace -> placeEntity model time
                | TryPush -> pushEntity model time
                | TryMultiPlace true -> multiPlaceEntity model time
                | TryMultiPlace false -> endMultiPlace model
                | TryOrient facing -> orientEntity model facing
                | NoAction -> model

            let isCarrying = model.Player.Carrying.Length


            let (info: PhysicsInfo) =
                { Time = time
                  Dt = dt
                  PossibleObstacles = getCollidables model.World.Tiles }

            let player = Player.tick info model.Player

            let struct (maybeTarget, maybeFeetIndex) =
                calculateTargets model.World.Tiles model.World.Size player

            let tiles =
                updateWorldReactive model.World.Tiles model.World.TicksElapsed model.World.Size

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

            let newWorld =
                { model.World with
                    Dt = dt
                    Slow = slow
                    TimeElapsed = time
                    TicksElapsed = model.World.TicksElapsed + 1L
                    Tiles = tiles }

            let model =
                { model with
                    World = newWorld
                    CameraPos = newCameraPos
                    Player =
                        { player with
                            CarryingDelta = isCarrying - wasCarrying
                            TargetedTile = maybeTarget
                            Feet = maybeFeetIndex
                            Action = NoAction } }

            if goToNextLevel then nextLevel model else model
    | MultiPlaceEntity ->
        { model with
            Player = setPlayerAction (TryMultiPlace true) }
    | EndMultiPlace ->
        { model with
            Player = setPlayerAction (TryMultiPlace false) }


// VIEW

let targetColor (model: Model) (i: int) =
    let alpha = 0.5f //TODO move to config

    voption {
        let! (tile, ind) = model.Player.TargetedTile
        let! target = if i = ind then ValueSome tile else ValueNone
        let illegal = ValueOption.isSome target.Collider || ValueOption.isSome target.Entity

        return
            if illegal then
                (Color.Orange * alpha)
            else
                (Color.Green * alpha)
    }


let viewTargets
    (maybeTargetColor: voption<Color>)
    (model: Model)
    (i: int)
    (xPixel: int)
    (yPixel: int)
    (sourceRect: Rectangle)
    loadedAssets
    (spriteBatch: SpriteBatch)
    =
    let alpha = 0.5f //TODO move to config

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
            Rectangle(xPixel, yPixel, sourceRect.Width, sourceRect.Height),
            System.Nullable<Rectangle>(),
            color,
            0f,
            Vector2.Zero,
            effect,
            DepthConfig.Target
        )
    | ValueNone -> ()

    match model.Player.Feet with
    | ValueSome index when index = i ->
        spriteBatch.Draw(
            loadedAssets.textures["feet"],
            Rectangle(xPixel, yPixel, sourceRect.Width, sourceRect.Height),
            System.Nullable<Rectangle>(),
            Color.Green * alpha,
            0f,
            Vector2.Zero,
            effect,
            DepthConfig.Target
        )
    | _ -> ()


let viewWorldAndPlayer (model: Model) loadedAssets (spriteBatch: SpriteBatch) =
    let sourceRect = rect 0 0 blockWidth blockWidth
    let cameraOffset = -(halfScreenOffset model.CameraPos)

    spriteBatch.DrawString(loadedAssets.fonts["defaultFont"], model.World.LevelText, Vector2(0f, 0f), Color.White)

    model.World.Tiles
    |> PersistentVector.toSeq
    |> Seq.iteri (fun i tile ->
        let startX = 0
        let startY = 0
        let width = getWidth model

        let xBlockOffSet = (i % width) * blockWidth
        let yBlockOffSet = (i / width) * blockWidth

        let xPixel = startX + xBlockOffSet + int (cameraOffset.X)
        let yPixel = startY + yBlockOffSet + int (cameraOffset.Y)

        //floor
        viewFloor tile xPixel yPixel sourceRect loadedAssets spriteBatch
        let maybeTargetColor = targetColor model i
        viewTargets maybeTargetColor model i xPixel yPixel sourceRect loadedAssets spriteBatch
        viewEntities maybeTargetColor cameraOffset tile xPixel yPixel loadedAssets spriteBatch)

    Player.viewPlayer model.Player (halfScreenOffset model.CameraPos) loadedAssets spriteBatch


let view (model: Model) (dispatch: Message -> unit) loadedAssets _inputs spriteBatch =
    match model.World.Song with
    | PlaySong songName ->
        Media.MediaPlayer.Volume <- 0.4f
        //Media.MediaPlayer.Play(loadedAssets.music[songName])
        Media.MediaPlayer.IsRepeating <- true
        dispatch (SongStarted songName)
    | Stopped -> Media.MediaPlayer.Stop()
    | _ -> ()

    match model.LevelTransition with
    | ValueSome transition -> ()
    | ValueNone -> viewWorldAndPlayer model loadedAssets spriteBatch

let inputs (inputs: Inputs) (dispatch: Message -> unit) =
    if Controls.isKeyDown Keys.X inputs || Controls.isButtonDown Buttons.A inputs then
        (dispatch (PickUpEntity))

    if Controls.isKeyDown Keys.S inputs || Controls.isButtonDown Buttons.X inputs then
        (dispatch (PushEntity))

    if Controls.isKeyDown Keys.C inputs || Controls.isButtonDown Buttons.B inputs then
        (dispatch (PlaceEntity))

    if Controls.whileKeyDown Keys.C inputs || Controls.whileButtonDown Buttons.B inputs then
        (dispatch (MultiPlaceEntity))

    if Controls.isKeyUp Keys.C inputs || Controls.isButtonUp Buttons.B inputs then
        (dispatch (EndMultiPlace))

    if Controls.isKeyDown Keys.Z inputs || Controls.isButtonDown Buttons.Y inputs then
        (dispatch (Interact))

    Player.inputs inputs (PlayerMessage >> dispatch)
    dispatch (PhysicsTick(inputs.totalGameTime, inputs.gameTime.IsRunningSlowly))
