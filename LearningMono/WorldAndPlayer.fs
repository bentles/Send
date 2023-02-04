module World

open Xelmish.Model
open Xelmish.Viewables
open Microsoft.Xna.Framework
open GameConfig
open PlayerConfig
open Elmish
open Collision
open FsToolkit.ErrorHandling
open Utility
open Level
open Entity
open FSharpx.Collections
open Microsoft.Xna.Framework.Graphics
open Prelude


type Model =
    { Tiles: PersistentVector<Tile>

      Size: int * int

      Dt: float32
      Slow: bool
      TimeElapsed: int64

      //player and camera
      Player: Player.Model
      PlayerTarget: (Tile * int) option
      CameraPos: Vector2 }

let halfScreenOffset (camPos: Vector2) : Vector2 =
    Vector2.Subtract(camPos, Vector2(800f, 450f))

let getCollidables (tiles: Tile seq) : AABB seq =
    tiles
    |> Seq.choose (fun tile ->
        match tile.Collider with
        | Some collider -> Some collider
        | _ ->
            match tile.Entity with
            | Some { Collider = collider } -> collider
            | _ -> None)

let getTileAtPos (pos: Vector2) (size: int * int) (tiles: PersistentVector<Tile>) : (Tile * int) option =
    let coords = posToCoords pos
    let index = coordsToIndex coords size
    index |> Option.map (fun index -> PersistentVector.nth index tiles, index)

let init (worldConfig: WorldConfig) time =
    let level = level1 time

    { Tiles = level.Tiles
      Size = level.Size
      Player = Player.init level.PlayerStartsAtPos level.PlayerStartsCarrying playerConfig charSprite time
      Slow = false
      Dt = 0f
      PlayerTarget = None
      TimeElapsed = 0
      CameraPos = Vector2(0f, -0f) }


// UPDATE
type Message =
    | PlayerMessage of Player.Message
    | PickUpEntity
    | PlaceEntity
    | PhysicsTick of time: int64 * slow: bool

let updateWorldReactive (tiles: PersistentVector<Tile>) : PersistentVector<Tile> =
    tiles
    |> PersistentVector.map (fun tile ->
        let maybeEntity =
            option {
                let! entity = tile.Entity

                let newEntityType =
                    match entity.Type with
                    | Subject subject -> Subject((getSubjectFunc subject.Type) subject)
                    | Observable({ Type = oType
                                   Observing = ob1
                                   Observing2 = ob2 } as oData) ->

                        // get what is being is observed if anything
                        let getObserved (ob: int option) =
                            option {
                                let! i = ob
                                let tile = PersistentVector.nth i tiles
                                let! e = tile.Entity
                                return e.Type
                            }

                        let eType1 = getObserved ob1
                        let eType2 = getObserved ob2

                        Observable((getObserverFunc oType) oData eType1 eType2)
                    | other -> other

                return { entity with Type = newEntityType }
            }

        { tile with Entity = maybeEntity })

let updateWorldSprites (totalTime: int64) (tiles: PersistentVector<Tile>) : PersistentVector<Tile> =
    tiles
    |> PersistentVector.map (fun tile ->
        let entityy =
            option {
                let! entity = tile.Entity
                let (sprite, _) = (Sprite.update (Sprite.AnimTick totalTime) entity.Sprite)
                let r = ({ entity with Sprite = sprite })
                return r
            }

        match entityy with
        | Some(entity) -> { tile with Entity = Some entity }
        | None -> { tile with Entity = None })

let updateCameraPos (playerPos: Vector2) (oldCamPos: Vector2) : Vector2 =
    let diff = Vector2.Subtract(playerPos, oldCamPos)
    let halfDiff = Vector2.Multiply(diff, 0.25f)

    if halfDiff.LengthSquared() < 0.5f then
        playerPos
    else
        oldCamPos + halfDiff

let updatePlayer (message: Player.Message) (worldModel: Model) =
    let model = worldModel.Player

    match message with
    | Player.Input direction -> { model with Input = direction }, Cmd.none
    | Player.PlayerPhysicsTick info ->
        let newModel = Player.updatePhysics model info
        let aniCommands = Player.updateAnimations newModel model
        newModel, aniCommands
    | Player.RotatePlacement clock ->
        { model with PlacementFacing = rotateFacing model.PlacementFacing clock }, Cmd.none
    | Player.SpriteMessage sm ->
        let (newSprite, event) = Sprite.update sm model.SpriteInfo

        let model, cmd =
            match event with
            | Sprite.AnimationComplete _ ->
                let (newState, walkAni) = Player.transformComplete model.CharacterState

                let maxVelocity =
                    match newState with
                    | Player.Small s when s -> playerConfig.SmallMaxVelocity
                    | Player.Small s when not s -> playerConfig.BigMaxVelocity
                    | _ -> model.MaxVelocity

                let modl =
                    { model with
                        CharacterState = newState
                        MaxVelocity = maxVelocity }

                modl,
                (Cmd.ofMsg << Player.SpriteMessage << Sprite.SwitchAnimation) (walkAni, walkAni.Speed, modl.IsMoving)
            | Sprite.AnimationLooped _
            | Sprite.None -> model, Cmd.none

        { model with SpriteInfo = newSprite }, cmd
    | Player.CarryingMessage sm ->
        let newCarrying =
            model.Carrying
            |> List.map (fun carry ->
                let (newSprite, _) = Sprite.update sm carry.Sprite
                { carry with Sprite = newSprite })

        { model with Carrying = newCarrying }, Cmd.none
    | Player.TransformCharacter ->
        let (newState, transformAnimation) = Player.transformStart model.CharacterState

        { model with CharacterState = newState },
        (Cmd.ofMsg << Player.SpriteMessage << Sprite.SwitchAnimation) (transformAnimation, 100, true)
    | Player.FreezeMovement holding -> { model with MovementFrozen = holding }, Cmd.none
    | Player.ArrowsControlPlacement theyDo ->
        { model with
            MovementFrozen = theyDo
            ArrowsControlPlacement = theyDo },
        Cmd.none


let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let update (message: Message) (model: Model) : Model * Cmd<Message> =
    let player = model.Player

    match message with
    | PlayerMessage playerMsg ->
        let (newPlayerModel, playerCommand) = updatePlayer playerMsg model
        { model with Player = newPlayerModel }, Cmd.map PlayerMessage playerCommand
    | PickUpEntity ->
        let playerLimit = Player.getPlayerPickupLimit player.CharacterState

        match player.CharacterState with
        | Player.Small _ when player.Carrying.Length + 1 <= playerLimit ->
            option {
                let! (tile, i) = model.PlayerTarget

                let tiles = model.Tiles |> PersistentVector.update i { tile with Entity = None }

                let! entity = tile.Entity

                return
                    { model with
                        Tiles = tiles
                        Player = { player with Carrying = entity :: player.Carrying } },
                    Cmd.none
            }
            |> Option.defaultValue (model, Cmd.none)
        | _ -> model, Cmd.none
    | PlaceEntity ->
        match player.CharacterState with
        | Player.Small _ ->
            let tileAndIndex = model.PlayerTarget

            match tileAndIndex with
            | Some({ Entity = None } as tile, i) ->
                match player.Carrying with
                | entity :: rest ->
                    //make a targeting function
                    let roundedPos = posRounded player.Target worldConfig
                    let (x, y) = posToCoords roundedPos
                    let xface, yface = facingToCoords player.PlacementFacing
                    let at = (x + xface, y + yface)

                    let facing = player.PlacementFacing

                    let entityType = withTarget entity.Type (coordsToIndex at model.Size)
                    let entity = Entity.init entityType roundedPos model.TimeElapsed facing
                    let sprite, _ = Sprite.update Sprite.StartAnimation entity.Sprite
                    let entity = { entity with Sprite = sprite }

                    let tiles =
                        model.Tiles |> PersistentVector.update i { tile with Entity = Some(entity) }

                    { model with
                        Tiles = tiles
                        Player = { player with Carrying = rest } },
                    Cmd.none
                | _ -> model, Cmd.none
            | _ -> model, Cmd.none
        | _ -> model, Cmd.none
    | PhysicsTick(time, slow) ->
        //TODO: get a list of things the player could interact with
        let dt = (float32 (time - lastTick)) / 1000f
        lastTick <- time

        let (info: PhysicsInfo) =
            { Time = time
              Dt = dt
              PossibleObstacles = getCollidables model.Tiles }

        let player, playerMsg = updatePlayer (Player.PlayerPhysicsTick info) model
        let tileAndIndex = getTileAtPos player.Target model.Size model.Tiles

        let tiles = updateWorldReactive model.Tiles

        //TODO: move this outside??
        let tiles = updateWorldSprites time tiles

        let newCameraPos = updateCameraPos player.Pos model.CameraPos

        { model with
            Dt = dt
            Slow = slow
            TimeElapsed = time
            Tiles = tiles
            CameraPos = newCameraPos
            Player = player
            PlayerTarget = tileAndIndex },
        Cmd.batch [ Cmd.map PlayerMessage playerMsg ]

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


let viewWorld (model: Model) (worldConfig: WorldConfig) =
    let blockWidth = worldConfig.TileWidth
    let empty = "tile"
    let grass = "grass"

    let sourceRect = rect 0 0 blockWidth blockWidth
    let cameraOffset = -(halfScreenOffset model.CameraPos)

    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->
        seq { 0 .. (model.Tiles.Length - 1) }
        |> Seq.iter (fun i ->
            let tile = model.Tiles[i]

            let texture =
                match tile.FloorType with
                | FloorType.Grass -> grass
                | FloorType.Empty -> empty

            let startX = 0
            let startY = 0
            let width, height = model.Size

            let xBlockOffSet = (i % width) * blockWidth
            let yBlockOffSet = (i / height) * blockWidth

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
                option {
                    let! (tile, ind) = model.PlayerTarget
                    let! target = if i = ind then Some tile else None
                    let illegal = Option.isSome target.Collider || Option.isSome target.Entity

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
            |> Option.iter (fun color ->
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
            |> Option.iter (fun (entity: Entity.Model) ->
                Sprite.drawSpriteInner
                    entity.Sprite
                    -cameraOffset
                    loadedAssets.textures[entity.Sprite.CurrentImage.TextureName]
                    spriteBatch)


            match tile.Entity with
            | Some({ Type = EmittingedObservable(etype, t) }) ->
                viewEmitting
                    etype
                    t
                    (actualX, actualY)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage etype).TextureName]
            | _ -> ()))

let viewPlayer (model: Player.Model) (cameraPos: Vector2) (dispatch: Player.Message -> unit) =
    seq {
        //input
        yield directions Keys.Up Keys.Down Keys.Left Keys.Right (fun f -> dispatch (Player.Input f))
        // yield directions Keys.W Keys.S Keys.A Keys.D (fun f -> dispatch (Input f))
        yield onkeydown Keys.Space (fun _ -> dispatch (Player.TransformCharacter))
        yield onkeydown Keys.LeftControl (fun _ -> dispatch (Player.FreezeMovement true))
        yield onkeyup Keys.LeftControl (fun _ -> dispatch (Player.FreezeMovement false))

        yield onkeydown Keys.LeftAlt (fun _ -> dispatch (Player.ArrowsControlPlacement true))
        yield onkeyup Keys.LeftAlt (fun _ -> dispatch (Player.ArrowsControlPlacement false))

        yield onkeydown Keys.A (fun _ -> dispatch (Player.RotatePlacement false))
        yield onkeydown Keys.S (fun _ -> dispatch (Player.RotatePlacement true))

        //render
        yield! Sprite.view model.SpriteInfo cameraPos (Player.SpriteMessage >> dispatch)
        yield! Player.viewCarrying model.Carrying cameraPos model.CharacterState

    //debug
    //yield
    //    debugText
    //        $"pos:{model.Pos.X}  {model.Pos.Y}\ninput:{model.Input.X}  {model.Input.Y} \nfacing:{model.Facing.X}  {model.Facing.Y}"
    //        (40, 300)
    //yield renderAABB (collider model.Pos model.CollisionInfo) cameraPos
    }

let view model (dispatch: Message -> unit) =
    seq {
        // input
        yield onkeydown Keys.Z (fun _ -> dispatch (PickUpEntity))
        yield onkeydown Keys.X (fun _ -> dispatch (PlaceEntity))

        yield
            onupdate (fun input ->
                let mousePos = input.mouseState.Position
                ())

        // physics
        yield onupdate (fun input -> dispatch (PhysicsTick(input.totalGameTime, input.gameTime.IsRunningSlowly)))

        //rende
        yield viewWorld model worldConfig
        yield! viewPlayer model.Player (halfScreenOffset model.CameraPos) (PlayerMessage >> dispatch)

    //debug
    // ok this is a complete lie since the timestep is fixed
    //  yield debugText $"running slow?:{model.Slow}" (40, 100)
    }
