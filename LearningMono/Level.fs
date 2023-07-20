module Level

open Microsoft.Xna.Framework
open Entity
open Utility
open Prelude
open FSharpx.Collections

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

// helpers
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

let updateTilesWithEntity (tiles:Tiles) (i:int) (tile:Tile) (entity:Entity.Model) : Tiles =
    let newTile = { tile with Entity = ValueSome entity }
    tiles |> PersistentVector.update i newTile