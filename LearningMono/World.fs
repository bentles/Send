module World

open Xelmish.Model
open Xelmish.Viewables
open Microsoft.Xna.Framework
open Config
open Elmish
open Collision

let coordsToPos (xx: float32) (yy: float32) (half: Vector2) =
    let startX = 0f
    let startY = 0f

    let xBlockOffSet = float32 (xx * float32 worldConfig.TileWidth)
    let yBlockOffSet = float32 (yy * float32 worldConfig.TileWidth)

    let actualX = startX + xBlockOffSet + half.X
    let actualY = startY + yBlockOffSet + half.Y

    Vector2(actualX, actualY)

let posToCoords (pos:Vector2) : (int * int) =
    let x = int (pos.X / float32 worldConfig.TileWidth)
    let y = int (pos.Y / float32 worldConfig.TileWidth)
    (x, y)

let createColliderFromCoords (xx: float32) (yy: float32) (half: Vector2) =
    { Pos = coordsToPos xx yy half
      Half = half }

type FloorType =
    | Empty
    | Grass

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
      Player: Player.Model
      CameraPos: Vector2 }

type Message =
    | PlayerMessage of Player.Message
    | PhysicsTick of time: int64

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
            | None -> None
            | Some entity ->
                match entity.Collider with
                | Some collider -> Some collider
                | None -> None)

let getEntityAtPos (pos:Vector2) (tiles:Tile[]) : Entity.Model option =
    let (x,y) = posToCoords pos    
    if x > worldConfig.WorldTileLength || y > worldConfig.WorldTileLength then
        None
    else
        let index = y * worldConfig.WorldTileLength + x
        tiles[index].Entity

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
          Entity = Some(Entity.init timerSpriteConfig pos (Vector2(10f, 10f)) Vector2.Zero) }

    let createObserverOnGrass (coords: Vector2) =
        let pos = coordsToPos coords.X coords.Y half

        { FloorType = FloorType.Grass
          Collider = None
          Entity = Some(Entity.init observerSpriteConfig pos (Vector2(10f, 10f)) Vector2.Zero) }

    let blocks =
        [| for yy in 0 .. (worldConfig.WorldTileLength - 1) do
               for xx in 0 .. (worldConfig.WorldTileLength - 1) do
                   let grassTile = createNonCollidableTile FloorType.Grass

                   match xx, yy with
                   | 0, 0 -> createNonCollidableTile FloorType.Grass
                   | 2, 2 -> createTimerOnGrass (Vector2(2f))
                   | 3, 3 -> createObserverOnGrass (Vector2(3f))
                   | 5, 5 -> grassTile// 5f 5f
                   | 5, 6 -> grassTile// 5f 6f
                   | 7, 9 -> grassTile// 7f 9f
                   | 8, 9 -> grassTile// 8f 9f
                   | 6, 9 -> grassTile// 6f 9f
                   | 7, 8 -> grassTile// 7f 8f
                   | x, y -> grassTile |]

    { Tiles = blocks
      ChunkBlockLength = worldConfig.WorldTileLength
      TileWidth = worldConfig.TileWidth
      Player = Player.init 0 0 playerConfig charSprite
      CameraPos = Vector2(0f, -0f) }


let update (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | PlayerMessage playerMsg ->
        let (newPlayerModel, playerCommand) = Player.update playerMsg model.Player
        { model with Player = newPlayerModel }, Cmd.map PlayerMessage playerCommand
    | PhysicsTick time ->
        //TODO: get a list of things the player could interact with
        let (info: Player.PhysicsInfo) =
            { Time = time
              PossibleObstacles = getCollidables model.Tiles }

        let player, playerMsg = Player.update (Player.PhysicsTick info) model.Player
        let newCameraPos = updateCameraPos player.Pos model.CameraPos

        { model with
            CameraPos = newCameraPos
            Player = player },
        Cmd.map PlayerMessage playerMsg

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

            match entity with
            | Some s ->
                yield floor
                yield! s
            | None -> yield floor
    }

let view model (dispatch: Message -> unit) =
    [ yield onupdate (fun input -> dispatch (PhysicsTick input.totalGameTime))

      yield! renderWorld model
      yield! Player.view model.Player (halfScreenOffset model.CameraPos) (PlayerMessage >> dispatch) ]
