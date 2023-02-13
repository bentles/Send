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
    { Tiles: PersistentVector<Tile>
      Song: SongState

      Size: int * int

      Dt: float32
      Slow: bool
      TimeElapsed: int64

      //player and camera
      PlayerAction: PlayerWorldInteraction
      Player: Player.Model
      PlayerTarget: struct (Tile * int) voption
      CameraPos: Vector2 }

let halfScreenOffset (camPos: Vector2) : Vector2 =
    Vector2.Subtract(camPos, Vector2(800f, 450f))

let getCollidables (tiles: Tile seq) : AABB seq =
    tiles
    |> Seq.choose  (fun tile ->
        match tile.Collider with
        | ValueSome collider -> Some collider
        | _ ->
            match tile.Entity with
            | ValueSome { Collider = ValueSome collider } -> Some collider
            | _ -> None)

let getTileAtPos (pos: Vector2) (size: int * int) (tiles: PersistentVector<Tile>) : struct (Tile * int) voption =
    let coords = vectorToCoords pos
    let index = coordsToIndex coords size
    index |> ValueOption.map (fun index -> PersistentVector.nth index tiles, index)

let init time =
    let level = level3 time

    { Tiles = level.Tiles
      Song = PlaySong "tutorial"
      Size = level.Size
      PlayerAction = NoAction
      Player = Player.init level.PlayerStartsAtPos level.PlayerStartsCarrying playerConfig charSprite time
      Slow = false
      Dt = 0f
      PlayerTarget = ValueNone
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

let updateWorldReactive (tiles: PersistentVector<Tile>) : PersistentVector<Tile> =
    tiles
    |> PersistentVector.map (fun tile ->
        let maybeEntity =
            voption {
                let! entity = tile.Entity

                let newEntityType =
                    match entity.Type with
                    | Subject subject -> Subject((getSubjectFunc subject.Type) subject)
                    | Observable({ Type = oType
                                   Observing = ob1
                                   Observing2 = ob2 } as oData) ->

                        // get what is being is observed if anything
                        let getObserved (ob: int voption) =
                            voption {
                                let! i = ob
                                let tile = PersistentVector.nth i tiles
                                let! e = tile.Entity
                                return e.Type
                            }

                        let eType1 = getObserved ob1
                        let eType2 = getObserved ob2

                        Observable((getObserverFunc oType) oData eType1 eType2)
                    | other -> other

                // if emitting and has an onEmit fun then apply that
                let onEmit =
                    match newEntityType with
                    | EmittingObservable _ ->
                        let pos = (coordsToVector tile.Coords.X tile.Coords.Y half)
                        getOnEmit newEntityType pos
                    | _ -> id

                return onEmit { entity with Type = newEntityType }
            }

        { tile with Entity = maybeEntity })

let updateWorldSprites (totalTime: int64) (tiles: PersistentVector<Tile>) : PersistentVector<Tile> =
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
            let! entity = tile.Entity

            if entity.CanBePickedUp then
                let tiles = model.Tiles |> PersistentVector.update i { tile with Entity = ValueNone }

                return
                    { model with
                        Tiles = tiles
                        Player = { player with Carrying = entity :: player.Carrying } }

            else
                return! None
        }
        |> Option.defaultValue model
    | _ -> model

let placeEntity (model:Model): Model = 
    let player = model.Player
    match player.CharacterState with
        | Player.Small _ ->
            let tileAndIndex = model.PlayerTarget

            match tileAndIndex with
            | ValueSome({ Entity = ValueNone } as tile, i) ->
                match player.Carrying with
                | entity :: rest ->
                    //make a targeting function
                    let roundedPos = posRounded player.Target worldConfig
                    let (x, y) = vectorToCoords roundedPos
                    let xface, yface = facingToCoords player.PlacementFacing
                    let at = (x + xface, y + yface)

                    let facing = player.PlacementFacing

                    let entityType = withTarget entity.Type (coordsToIndex at model.Size)
                    let entity = Entity.init entityType roundedPos model.TimeElapsed facing true
                    let sprite = Sprite.startAnimation entity.Sprite
                    let entity = { entity with Sprite = sprite }

                    let tiles =
                        model.Tiles |> PersistentVector.update i { tile with Entity = ValueSome(entity) }

                    { model with
                        Tiles = tiles
                        Player = { player with Carrying = rest } }
                | _ -> model
            | _ -> model
        | _ -> model

let changeLevel (model:Model) (levelBuilder:LevelBuilder): Model =
    let newLevel = levelBuilder model.TimeElapsed

    { model with
        Size = newLevel.Size
        Tiles = newLevel.Tiles
        Player =
            { model.Player with
                Pos = newLevel.PlayerStartsAtPos
                Carrying = newLevel.PlayerStartsCarrying } }

let interactions (event: Entity.InteractionEvent) (model:Model) : Model =
    match event with
    | GoToLevel l ->
        changeLevel model (levelLookup l)
        
    | NoEvent -> model

let update (message: Message) (model: Model) : Model =
    match message with
    | PlayerMessage playerMsg ->
        let (newPlayerModel) = Player.update playerMsg model.Player
        { model with Player = newPlayerModel }
    | SongStarted name -> { model with Song = PlayingSong name }
    | PickUpEntity ->
        { model with PlayerAction = TryPickup }
    | PlaceEntity ->
        { model with PlayerAction = TryPlace }
    | Interact ->
        let maybeUpdate =
            voption {
                let! (tile, i) = model.PlayerTarget
                let! entity = tile.Entity
                let newEntity, event = Entity.interact entity
                let model = interactions event model

                let tiles =
                    model.Tiles |> PersistentVector.update i { tile with Entity = ValueSome newEntity }

                return { model with Tiles = tiles }
            }

        match maybeUpdate with
        | ValueNone -> model
        | ValueSome mnodel -> model
    
    | PhysicsTick(time, slow) ->
        let wasCarrying = model.Player.Carrying.Length

        let model = 
            match model.PlayerAction with
            | TryPickup ->
                pickUpEntity model
            | TryPlace ->
                placeEntity model
            | NoAction -> model

        let isCarrying = model.Player.Carrying.Length

        let dt = (float32 (time - lastTick)) / 1000f
        lastTick <- time

        let (info: PhysicsInfo) =
            { Time = time
              Dt = dt
              PossibleObstacles = getCollidables model.Tiles }

        let player = Player.tick info model.Player
        let tileAndIndex = getTileAtPos player.Target model.Size model.Tiles

        let tiles = updateWorldReactive model.Tiles
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
            PlayerTarget = tileAndIndex
            PlayerAction = NoAction
             }

// VIEW

let viewEmitting
    (entityType: EntityType)
    (ticksSinceLast: int)
    pos
    (spriteBatch: SpriteBatch)
    (texture: Graphics.Texture2D)
    =
    let imageInfo = getEmitImage entityType
    let (width, height) = (imageInfo.SpriteSize)

    if ticksSinceLast < 20 then
        let alpha = int ((float32 (30 - ticksSinceLast) / 20f) * 220f)
        let dwidth, dheight = (int ((float width) / 1.5), int ((float height) / 1.5))
        let x, y = pos

        spriteBatch.Draw(
            texture,
            Rectangle(x + 20, y - 5, dwidth, dheight),
            Rectangle(0, 0, width, height),
            (Color.FromNonPremultiplied(255, 255, 255, alpha))
        )
    else
        ()


let blockWidth = worldConfig.TileWidth
let empty = "tile"
let grass = "grass"
let wall = "wall"
let leftWall = "leftWall"
let rightWall = "rightWall"
let topWall = "topWall"
let bottomWall = "bottomWall"

let viewWorld (model: Model) (worldConfig: WorldConfig) =

    let sourceRect = rect 0 0 blockWidth blockWidth
    let cameraOffset = -(halfScreenOffset model.CameraPos)

    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->
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
            let width, _ = model.Size

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

            let texture, effect =
                match model.Player.PlacementFacing with
                | FacingUp -> "facingUp", SpriteEffects.None
                | FacingRight -> "facingRight", SpriteEffects.None
                | FacingDown -> "facingUp", SpriteEffects.FlipVertically
                | FacingLeft -> "facingRight", SpriteEffects.FlipHorizontally

            maybeTargetColor
            |> ValueOption.iter (fun color ->
                spriteBatch.Draw(
                    loadedAssets.textures[texture],
                    Rectangle(actualX, actualY, sourceRect.Width, sourceRect.Height),
                    System.Nullable<Rectangle>(),
                    color,
                    0f,
                    Vector2.Zero,
                    effect,
                    0f
                ))

            tile.Entity
            |> ValueOption.iter (fun (entity: Entity.Model) ->
                Sprite.drawSprite entity.Sprite -cameraOffset loadedAssets spriteBatch)

            match tile.Entity with
            | ValueSome({ Type = EmittingObservable(_, _) }) -> loadedAssets.sounds[ "click" ].Play(1f, 0.0f, 0.0f) |> ignore
            | _ -> ()

            match tile.Entity with
            | ValueSome({ Type = EmittingedObservable(etype, t) }) ->

                viewEmitting
                    etype
                    t
                    (actualX, actualY)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage etype).TextureName]
            | _ -> ()))

let view model (dispatch: Message -> unit) =
    seq {
        // input
        yield onkeydown Keys.X (fun _ -> dispatch (PickUpEntity))
        yield onkeydown Keys.C (fun _ -> dispatch (PlaceEntity))
        yield onkeydown Keys.Z (fun _ -> dispatch (Interact))

        yield
            onupdate (fun input ->
                let mousePos = input.mouseState.Position
                ())

        // music
        yield
            OnDraw(fun loaded _inputs _spriteBatch ->
                match model.Song with
                | PlaySong songName ->
                    // Media.MediaPlayer.Play(loaded.music[songName])
                    // Media.MediaPlayer.IsRepeating <- true
                    dispatch (SongStarted songName)
                | Stopped -> Media.MediaPlayer.Stop()
                | _ -> ())

        // physics
        yield onupdate (fun input -> dispatch (PhysicsTick(input.totalGameTime, input.gameTime.IsRunningSlowly)))

        //render
        yield viewWorld model worldConfig
        yield! Player.view model.Player (halfScreenOffset model.CameraPos) (PlayerMessage >> dispatch)

    //debug
    //  yield debugText $"running slow?:{model.Slow}" (40, 100)
    }
