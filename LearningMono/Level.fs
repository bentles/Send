module Level

open Microsoft.Xna.Framework
open Entity
open Utility
open Prelude
open FSharpx.Collections
open GameConfig
open Microsoft.Xna.Framework.Graphics
open Xelmish.Model

// Level primitives
[<Struct>]
type FloorType =
    | Empty
    | Void
    | Grass
    | Wall
    | TopWall
    | BottomWall
    | LeftWall
    | RightWall
    | Floor

[<Struct>]
type Tile =
    { FloorType: FloorType
      Collider: AABB voption
      Coords: Coords
      Entity: Entity.Model voption }

let defaultTile =
    { FloorType = FloorType.Empty
      Collider = ValueNone
      Coords = (0, 0)
      Entity = ValueNone }

type Tiles = PersistentVector<Tile>

type LevelData =
    { PlayerStartsAtPos: Vector2
      PlayerStartsCarrying: Entity.Model list
      LevelText: string
      Tiles: Tiles
      Size: Coords }

type LevelBuilder = int64 -> LevelData

[<Struct>]
type SongState =
    | PlaySong of song: string
    | PlayingSong of playing: string
    | Stopped

type World =
    { Tiles: Tiles
      Song: SongState
      LevelText: string
      Level: int
      Size: Coords
      Dt: float32
      Slow: bool
      TimeElapsed: int64
      TicksElapsed: int64 }

// helpers
let getIndexAtPos (pos: Vector2) (size: Coords) : int voption =
    let coords = offsetVectorToCoords pos
    coordsToIndex coords size

let getTileAtIndex index (tiles: Tiles) = PersistentVector.nth index tiles

let getTileAtPos (pos: Vector2) (size: Coords) (tiles: Tiles) : struct (Tile * int) voption =
    let index = getIndexAtPos pos size
    index |> ValueOption.map (fun index -> getTileAtIndex index tiles, index)

let createCollidableTile t (coords: Coords) =
    { defaultTile with
        Coords = coords
        FloorType = t
        Collider = ValueSome(createColliderFromCoords (toCoordsF coords) half) }

let createNonCollidableTile t (coords: Coords) =
    { defaultTile with
        FloorType = t
        Coords = coords }

let createEntityOn (entityType: EntityType) (floor: FloorType) (time: int64) (canBePickedUp: bool) (coords: Coords) =
    let pos = CoordsFToOffsetVector (toCoordsF coords) half

    { defaultTile with
        Coords = coords
        FloorType = floor
        Entity = ValueSome(Entity.init entityType pos time FacingRight canBePickedUp) }

let createRockOnGrass time (canBePickedUp: bool) (coords: Coords) =
    createEntityOn Rock Grass time canBePickedUp coords

let createSubjectOnGrass (subjectType: SubjectType) time pickedUp (coords: Coords) =
    createEntityOn
        (Entity.Subject
            { Type = subjectType
              TicksSinceEmit = 0
              GenerationNumber = 0
              ToEmit = Nothing })
        (if pickedUp then Grass else Floor)
        time
        pickedUp
        coords

let createTimerOnGrass time pickedUp (coords: Coords) =
    createSubjectOnGrass
        (Entity.Timer(
            { Items = [ Rock; buildObserver Id; buildObserver (Map Rock); rockTimer ]
              IsOpen = false },
            60
        ))
        time
        pickedUp
        coords

let createButtonOnGrass time pickedUp (coords: Coords) =
    createSubjectOnGrass Entity.Button time pickedUp coords

let observerEntity observer =
    Entity.init observer Vector2.Zero 0 FacingLeft true

let observerOnGrass time observer facing canPickup (coords: Coords) : Tile =
    let pos = CoordsFToOffsetVector (toCoordsF coords) half

    { defaultTile with
        Coords = coords
        FloorType = (if canPickup then FloorType.Grass else FloorType.Floor)
        Entity = ValueSome(Entity.init observer pos time facing canPickup) }

let observerCanPick time observer facing (coords: Coords) : Tile =
    let pos = CoordsFToOffsetVector (toCoordsF coords) half

    { defaultTile with
        Coords = coords
        FloorType = FloorType.Grass
        Entity = ValueSome(Entity.init observer pos time facing true) }

let observerCannotPick time observer facing (coords: Coords) : Tile =
    let pos = CoordsFToOffsetVector (toCoordsF coords) half

    { defaultTile with
        Coords = coords
        FloorType = FloorType.Floor
        Entity = ValueSome(Entity.init observer pos time facing false) }

let worldVars x y height width =
    (float32 x, float32 y, height - 1, width - 1)
    
let worldFromTemplate (template: List<List<Coords -> Tile>>) =
    seq {
        for y, row in List.indexed (template) do
            for x, tileBuilder in List.indexed (row) do
                let loc = struct (x, y)
                tileBuilder loc
    }
    |> PersistentVector.ofSeq,
    template.Head.Length,
    template.Length

// VIEW
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

let viewFloor (tile:Tile) (xPixel:int) (yPixel:int) (sourceRect:Rectangle) (loadedAssets:LoadedAssets) (spriteBatch: SpriteBatch) = 
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

    
    spriteBatch.Draw(
        loadedAssets.textures[texture],
        Rectangle(xPixel, yPixel, sourceRect.Width, sourceRect.Height),
        Color.White
    )

let viewEntities (maybeTargetColor:voption<Color>) (cameraOffset:Vector2) (tile:Tile) (xPixel:int) (yPixel:int) (loadedAssets:LoadedAssets) (spriteBatch: SpriteBatch) = 
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

            match entity.Type with
            | EmittingObservable(_, _) -> loadedAssets.sounds["click"].Play(0.05f, 0.0f, 0.0f) |> ignore
            | _ -> ()

            match entity.Type with
            | RenderEmittingObservable(etype, t) ->
                viewEmitting
                    etype
                    t
                    (xPixel, yPixel)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage etype).TextureName]
            | _ -> ()

            match entity.Type, maybeTargetColor with
            | CanPickOutOfEntity(_, eType), ValueSome _ ->
                viewObserverItem
                    eType
                    1
                    (xPixel, yPixel)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage eType).TextureName]
            | CanPlaceIntoEntity Unit (_), ValueSome _ ->
                viewObserverItem
                    Unit
                    1
                    (xPixel, yPixel)
                    spriteBatch
                    loadedAssets.textures[(getEmitImage Unit).TextureName]
            | _ -> ()

        | ValueNone -> ()

    match tile.Entity with
        | ValueSome { Collider = ValueSome collider } ->
            Collision.viewAABB collider (-cameraOffset) loadedAssets spriteBatch
        | _ -> ()

