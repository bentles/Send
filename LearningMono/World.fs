module World

open Xelmish.Model
open Xelmish.Viewables
open Microsoft.Xna.Framework
open Config
open Elmish
open Collision
open Debug
open Input
open FsToolkit.ErrorHandling
open Player
open Utility
open LevelConfig
open Entity
open System


type Model =
    { Tiles: Tile[]

      Dt: float32
      Slow: bool
      TimeElapsed: int64

      //player and camera
      Player: PlayerModel
      PlayerTarget: (Tile * int) option
      CameraPos: Vector2 }

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


let inputAffectsVelocityAssertions (input: Vector2) (oldVel: Vector2) (newVel: Vector2) : bool =
    if input = Vector2.Zero then
        newVel.Length() <= oldVel.Length() + AcceptableError
    else
        Vector2.Dot(input, newVel) >= Vector2.Dot(input, newVel) - AcceptableError

let updateCarryingPositions (pos: Vector2) =
    Cmd.ofMsg (CarryingMessage(Sprite.Message.SetPos pos))


let updatePlayerPhysics model (info: PhysicsInfo) =
    let dt = info.Dt

    // record when last x and y were pressed
    let xinputTime, lastXDir =
        if model.Input.X <> 0f then
            info.Time, model.Input.X
        else
            model.XInputTimeAndDir

    let yinputTime, lastYDir =
        if model.Input.Y <> 0f then
            info.Time, model.Input.Y
        else
            model.YInputTimeAndDir

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

    let dx = (float32 (info.Time - xinputTime)) / 1000f
    let dy = (float32 (info.Time - yinputTime)) / 1000f

    let minTime = 0.08f
    let diagonal = abs (dx - dy) < minTime

    let facing =
        match (model.Input.X, model.Input.Y) with
        | (0f, 0f) when diagonal -> Vector2(lastXDir, lastYDir)
        | (0f, 0f) when dx < dy -> Vector2(lastXDir, 0f)
        | (0f, 0f) when dy <= dx -> Vector2(0f, lastYDir)

        | (0f, 1f) when dx < minTime -> Vector2(lastXDir, 1f)
        | (1f, 0f) when dy < minTime -> Vector2(1f, lastYDir)
        | (0f, -1f) when dx < minTime -> Vector2(lastXDir, -1f)
        | (-1f, 0f) when dy < minTime -> Vector2(-1f, lastYDir)
        | _ -> model.Input

    let facing = Vector2.Normalize(facing)
    let target = pos + (60f * facing) + Vector2(0f, 20f)

    let (vel, pos, isMoving) =
        if model.Holding then
            (Vector2.Zero, model.Pos, false)
        else
            (vel, pos, velLength > 0f)

    { model with
        Target = target
        XInputTimeAndDir = xinputTime, lastXDir
        YInputTimeAndDir = yinputTime, lastYDir
        Facing = facing
        Vel = vel
        Pos = pos
        IsMoving = isMoving }

let updatePlayerAnimations (newModel:PlayerModel) (oldModel:PlayerModel) =
    let directionCommands =
        if newModel.Facing.X <> oldModel.Facing.X || newModel.Facing.Y <> oldModel.Facing.Y then
            [ Cmd.ofMsg (SpriteMessage(Sprite.SetDirection(newModel.Facing.X < 0f, newModel.Facing.Y < 0f))) ]
        else
            []

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

let transformStart (characterState: CharacterState) =
    match characterState with
    | Shrinking -> Growing, CharAnimations.SmallToBig
    | Small true -> Growing, CharAnimations.SmallToBig
    | Growing -> Shrinking, CharAnimations.BigToSmall
    | Small false -> Shrinking, CharAnimations.BigToSmall

let transformComplete (characterState: CharacterState) =
    match characterState with
    | Shrinking -> Small true, CharAnimations.SmallWalk
    | Growing -> Small false, CharAnimations.BigWalk
    | Small true -> Small true, CharAnimations.SmallWalk
    | Small false -> Small false, CharAnimations.BigWalk

let viewPlayerCarrying (carrying: Entity.Model list) (cameraPos: Vector2) (charState: CharacterState) =
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

let updateCameraPos (playerPos: Vector2) (oldCamPos: Vector2) : Vector2 =
    let diff = Vector2.Subtract(playerPos, oldCamPos)
    let halfDiff = Vector2.Multiply(diff, 0.25f)

    if halfDiff.LengthSquared() < 0.5f then
        playerPos
    else
        oldCamPos + halfDiff

let halfScreenOffset (camPos: Vector2) : Vector2 =
    Vector2.Subtract(camPos, Vector2(800f, 450f))

let getCollidables (tiles: Tile[]) : AABB seq =
    tiles
    |> Seq.choose (fun tile ->
        match tile.Collider with
        | Some collider -> Some collider
        | _ ->
            match tile.Entity with
            | Some { Collider = collider } -> collider
            | _ -> None)

let getTileAtPos (pos: Vector2) (tiles: Tile[]) : (Tile * int) option =
    let coords = posToCoords pos
    let index = coordsToIndex coords
    index |> Option.map (fun index -> tiles[index], index)

let init (worldConfig: WorldConfig) time =
    let blocks =
        [| for yy in 0 .. (worldConfig.WorldTileLength - 1) do
               for xx in 0 .. (worldConfig.WorldTileLength - 1) do
                   let grassTile = createNonCollidableTile FloorType.Grass
                   let subj = Some 22

                   match xx, yy with
                   | 0, 0 -> createNonCollidableTile FloorType.Grass
                   | 2, 2 -> createTimerOnGrass (Vector2(2f)) time
                   | 3, 3 -> createObserverOnGrass (Vector2(3f)) time (onlyRockFilter subj)
                   | 5, 5 -> createCollidableTile FloorType.Empty 5f 5f
                   | 5, 6 -> createObserverOnGrass (Vector2(5f, 6f)) time (idObservable subj)
                   | 7, 9 -> createObserverOnGrass (Vector2(7f, 9f)) time (mapToTimer (Some 33))
                   | 8, 9 -> grassTile // 8f 9f
                   | 6, 9 -> grassTile // 6f 9f
                   | 7, 8 -> grassTile // 7f 8f
                   | x, y -> grassTile |] // /* createTimerOnGrass (Vector2(float32 x, float32 y)) */ |]

    { Tiles = blocks
      Player = initPlayer 0 0 playerConfig charSprite time
      Slow = false
      Dt = 0f
      PlayerTarget = None
      TimeElapsed = 0
      CameraPos = Vector2(0f, -0f) }


// UPDATE
type Message =
    | PlayerMessage of PlayerMessage
    | PickUpEntity
    | PlaceEntity
    | PhysicsTick of time: int64 * slow: bool

let updateWorldReactive (tiles: Tile[]) : Tile[] =
    tiles
    |> Array.map (fun tile ->
        let maybeEntity =
            option {
                let! entity = tile.Entity

                let newEntityType =
                    match entity.Type with
                    | Subject subject -> Subject(subject.Action subject)
                    | Observable({ Type = oType
                                   Observing = Some observing } as observable) ->
                        // get what is being is observed if anything
                        let observedTile = Array.item observing tiles

                        match observedTile.Entity with
                        | Some observedEntity -> Observable(observable.Action observable observedEntity.Type)
                        | None -> Observable observable
                    | other -> other

                return { entity with Type = newEntityType }
            }

        { tile with Entity = maybeEntity })

let updateWorldSprites (totalTime: int64) (tiles: Tile[]) : Tile[] =
    tiles
    |> Array.map (fun tile ->
        let entityy =
            option {
                let! entity = tile.Entity
                let (sprite, ev) = (Sprite.update (Sprite.AnimTick totalTime) entity.Sprite)
                let r = ({ entity with Sprite = sprite })
                return r
            }

        match entityy with
        | Some(entity) -> { tile with Entity = Some entity }
        | None -> { tile with Entity = None })

let updatePlayer (message: PlayerMessage) (worldModel: Model) =
    let model = worldModel.Player

    match message with
    | Input direction -> { model with Input = direction }, Cmd.none
    | PlayerPhysicsTick info ->
        let newModel = updatePlayerPhysics model info
        let aniCommands = updatePlayerAnimations newModel model
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

                modl, (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (walkAni, walkAni.Speed, modl.IsMoving)
            | Sprite.AnimationLooped _
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
        (Cmd.ofMsg << SpriteMessage << Sprite.SwitchAnimation) (transformAnimation, 100, true)
    | Hold holding -> { model with Holding = holding }, Cmd.none


let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let update (message: Message) (model: Model) : Model * Cmd<Message> =
    let player = model.Player

    match message with
    | PlayerMessage playerMsg ->
        let (newPlayerModel, playerCommand) = updatePlayer playerMsg model
        { model with Player = newPlayerModel }, Cmd.map PlayerMessage playerCommand
    | PickUpEntity ->
        let playerLimit = getPlayerPickupLimit player.CharacterState

        match player.CharacterState with
        | Small _ when player.Carrying.Length + 1 <= playerLimit ->
            option {
                let! (tile, i) = model.PlayerTarget

                model.Tiles[i] <- { tile with Entity = None }

                let! entity = tile.Entity
                return { model with Player = { player with Carrying = entity :: player.Carrying } }, Cmd.none
            }
            |> Option.defaultValue (model, Cmd.none)
        | _ -> model, Cmd.none
    | PlaceEntity ->
        match player.CharacterState with
        | Small _ ->
            let tileAndIndex = model.PlayerTarget

            match tileAndIndex with
            | Some({ Entity = None } as tile, i) ->
                match player.Carrying with
                | entity :: rest ->
                    //make a targeting function
                    let roundedPos = posRounded player.Target worldConfig
                    let (x, y) = posToCoords roundedPos
                    
                    let xface, yface = round player.Facing.X, round player.Facing.Y
                    let at = (x + xface, y + yface)

                    let facing = 
                        match xface, yface with
                        | (-1,0) -> Entity.FacingLeft
                        | (1,0) -> Entity.FacingRight
                        | (0,-1) -> Entity.FacingUp
                        | (0,1) -> Entity.FacingDown
                        | (_,-1) -> Entity.FacingUp
                        | (_,1) -> Entity.FacingDown
                        | _ -> Entity.FacingRight

                    let entityType = withTarget entity.Type (coordsToIndex at)
                    let entity = Entity.init entityType roundedPos model.TimeElapsed facing
                    let sprite, ev = Sprite.update Sprite.StartAnimation entity.Sprite 
                    let entity = { entity with Sprite = sprite }

                    model.Tiles[i] <- { tile with Entity = Some(entity) } //TODO: no mutation

                    { model with Player = { player with Carrying = rest } }, Cmd.none
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

        let player, playerMsg = updatePlayer (PlayerPhysicsTick info) model
        let tileAndIndex = getTileAtPos player.Target model.Tiles

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

let viewEmitting (entityType: EntityType) (ticksSinceLast: int) pos =
    let imageInfo = getEmitImage entityType
    let (imageWidth, imageHeight) = (imageInfo.PixelSize)
    let width, height = (imageWidth / imageInfo.Columns, imageHeight / imageInfo.Rows)

    if ticksSinceLast < 20 then
        let alpha = int ((float32 (30 - ticksSinceLast) / 20f) * 220f)
        let destSize = (int ((float width) / 1.5), int ((float height) / 1.5))
        let x, y = pos

        seq {
            imageWithSource
                imageInfo.TextureName
                (Color.FromNonPremultiplied(255, 255, 255, alpha))
                (width, height)
                (0, 0)
                destSize
                (x + 20, y - 5)
        }
    else
        Seq.empty

let viewWorld (model: Model) (worldConfig: WorldConfig) =
    let blockWidth = worldConfig.TileWidth
    let empty = "tile"
    let grass = "grass"

    let sourceRect = rect 0 0 blockWidth blockWidth
    let cameraOffset = -(halfScreenOffset model.CameraPos)

    seq {
        for i in 0 .. (model.Tiles.Length - 1) do
            let tile = model.Tiles[i]

            let texture =
                match tile.FloorType with
                | FloorType.Grass -> grass
                | FloorType.Empty -> empty

            let startX = 0
            let startY = 0

            let xBlockOffSet = (i % worldConfig.WorldTileLength) * blockWidth
            let yBlockOffSet = (i / worldConfig.WorldTileLength) * blockWidth

            let actualX = startX + xBlockOffSet + int (cameraOffset.X)
            let actualY = startY + yBlockOffSet + int (cameraOffset.Y)

            let color =
                option {
                    let! (tile, ind) = model.PlayerTarget
                    let! target = if i = ind then Some tile else None
                    let illegal = Option.isSome target.Collider || Option.isSome target.Entity
                    return if illegal then Color.Orange else Color.Green
                }
                |> Option.defaultValue Color.White

            let floor =
                image texture color (sourceRect.Width, sourceRect.Height) (actualX, actualY)

            let entity =
                tile.Entity
                |> Option.map (fun (entity: Entity.Model) -> Sprite.view entity.Sprite -cameraOffset (fun f -> ()))

            //let debug =
            //    tile.Collider
            //    |> Option.map (fun (b: AABB) ->
            //        image
            //            empty
            //            Color.Red
            //            (int (b.Half.X * 2f), int (b.Half.Y * 2f))
            //            (int (b.Pos.X - b.Half.X + cameraOffset.X), int (b.Pos.Y - b.Half.Y + cameraOffset.Y)))
            //    |> Option.toList

            //let entityDebug =
            //    tile.Entity
            //    |> Option.bind (fun e -> e.Collider)
            //    |> Option.map (fun (b: AABB) ->
            //        image
            //            empty
            //            Color.Red
            //            (int (b.Half.X * 2f), int (b.Half.Y * 2f))
            //            (int (b.Pos.X - b.Half.X + cameraOffset.X), int (b.Pos.Y - b.Half.Y + cameraOffset.Y)))
            //    |> Option.toList

            // lololol
            let emitting =
                match tile.Entity with
                | Some({ Type = EmittingedObservable (s, t) }) -> viewEmitting s t (actualX, actualY)
                | _ -> Seq.empty

            match entity with
            | Some s ->
                yield floor
                yield! s
                yield! emitting
            //yield! debug
            //yield! entityDebug
            | None -> yield floor
    }

let viewPlayer model (cameraPos: Vector2) (dispatch: PlayerMessage -> unit) =
    seq {
        //input
        yield directions Keys.Up Keys.Down Keys.Left Keys.Right (fun f -> dispatch (Input f))
       // yield directions Keys.W Keys.S Keys.A Keys.D (fun f -> dispatch (Input f))
        yield onkeydown Keys.Space (fun _ -> dispatch (TransformCharacter))
        yield onkeydown Keys.LeftControl (fun _ -> dispatch (Hold true))
        yield onkeyup Keys.LeftControl (fun _ -> dispatch (Hold false))

        //render
        yield! Sprite.view model.SpriteInfo cameraPos (SpriteMessage >> dispatch)
        yield! viewPlayerCarrying model.Carrying cameraPos model.CharacterState

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

        yield onupdate (fun input -> 
            let mousePos = input.mouseState.Position
            ()
            )

        // physics
        yield onupdate (fun input -> dispatch (PhysicsTick(input.totalGameTime, input.gameTime.IsRunningSlowly)))

        //render
        yield! viewWorld model worldConfig
        yield! viewPlayer model.Player (halfScreenOffset model.CameraPos) (PlayerMessage >> dispatch)

        //debug
        // ok this is a complete lie since the timestep is fixed
        yield debugText $"running slow?:{model.Slow}" (40, 100)
    }
