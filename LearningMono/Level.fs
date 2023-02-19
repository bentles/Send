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
    | Grass
    | Wall
    | TopWall
    | BottomWall
    | LeftWall
    | RightWall

[<Struct>]
type Tile =
    { FloorType: FloorType
      Collider: AABB voption
      Coords: Vector2
      Entity: Entity.Model voption }

let defaultTile =
    { FloorType = FloorType.Empty
      Collider = ValueNone
      Coords = Vector2.Zero
      Entity = ValueNone }

type LevelData =
    { PlayerStartsAtPos: Vector2
      PlayerStartsCarrying: Entity.Model list
      Tiles: PersistentVector<Tile>
      Size: (int * int) }

type LevelBuilder = int64 -> LevelData

// helpers
let createCollidableTile t x y =
    { defaultTile with
        Coords = Vector2(x, y)
        FloorType = t
        Collider = ValueSome(createColliderFromCoords x y half) }

let createNonCollidableTile t x y =
    { defaultTile with
        FloorType = t
        Coords = Vector2(x, y) }

let createEntityOn (entityType: EntityType) (floor:FloorType) (coords: Vector2) (time: int64)  (canBePickedUp: bool)  =
    let pos = coordsToVector coords.X coords.Y half

    { defaultTile with
        Coords = coords
        FloorType = floor
        Entity = ValueSome(Entity.init entityType pos time FacingRight canBePickedUp) }

let createRockOnGrass (coords: Vector2) time = createEntityOn Rock Grass coords time

let createSubjectOnGrass (subjectType: SubjectType) (coords: Vector2) time pickedUp =
    createEntityOn
        (Entity.Subject
            { Type = subjectType
              TicksSinceEmit = 0
              GenerationNumber = 0
              ToEmit = Nothing })
        Grass
        coords
        time
        pickedUp

let createTimerOnGrass (coords: Vector2) time pickedUp =
    createSubjectOnGrass
        (Entity.Timer({ Items = [ Rock; buildObserver Id; buildObserver (Map Rock); rockTimer ]; IsOpen = false }, 60))
        coords
        time
        pickedUp

let createButtonOnGrass (coords: Vector2) time pickedUp =
    createSubjectOnGrass (Entity.Button Rock) coords time pickedUp

let observerEntity observer =
    Entity.init observer Vector2.Zero 0 FacingLeft true

let observerOnGrass (coords: Vector2) time observer facing canPickup : Tile =
    let pos = coordsToVector coords.X coords.Y half

    { defaultTile with
        Coords = coords
        FloorType = FloorType.Grass
        Entity = ValueSome(Entity.init observer pos time facing canPickup) }

let iterWorld (width: int, height: int) (func: (int * int) -> Tile) : PersistentVector<Tile> =
    seq {
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                func (x, y)
    }
    |> PersistentVector.ofSeq

let (|Corner|_|) (bottom, right) (x, y) =
    match x, y with
    | 0, 0 -> Some()
    | x, y when y = bottom && x = right -> Some()
    | x, 0 when x = right -> Some()
    | 0, y when y = bottom -> Some()
    | _ -> None

let (|Wall|_|) (bottom, right) (x, y) =
    match x, y with
    | _, y when y = bottom -> Some BottomWall
    | _, y when y = 0 -> Some TopWall
    | x, _ when x = 0 -> Some LeftWall
    | x, _ when x = right -> Some RightWall
    | _ -> None

let worldVars x y height width =
    (float32 x, float32 y, height - 1, width - 1)

let level1: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let tiles =
            iterWorld (width, height) (fun (x, y) ->
                let (fx, fy, bottom, right) = worldVars x y height width

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall fx fy
                | Wall (bottom, right) wallType -> createCollidableTile wallType fx fy

                | 7, 7 -> createEntityOn (GoToLevelButton L2) Grass (Vector2(fx, fy)) time false
                //some kind of goal
                | _ -> createNonCollidableTile FloorType.Grass fx fy)

        { PlayerStartsAtPos = Vector2(150f, 150f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level2: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let tiles =
            iterWorld (width, height) (fun (x, y) ->
                let (fx, fy, bottom, right) = worldVars x y height width

                let rocks =
                    [ (2, 2)
                      (5, 5)
                      (7, 6)
                      (6, 6)
                      (6, 8)
                      (6, 6)
                      (7, 8)
                      (7, 7)
                      (8, 7)
                      (8, 6) ]

                let observers = [ (3, 3); (3, 4); (6, 7) ]

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall fx fy
                | Wall (bottom, right) wallType -> createCollidableTile wallType fx fy
                | xy when (List.contains xy rocks) -> createRockOnGrass (Vector2(fx, fy)) time true
                | xy when (List.contains xy observers) ->
                    observerOnGrass (Vector2(fx, fy)) time (buildObserver Id) FacingLeft true

                | 8, 8 -> createEntityOn (GoToLevelButton L3) Grass (Vector2(8f, 8f)) time false

                | _ -> createNonCollidableTile FloorType.Grass fx fy)

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }


let level3: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let tiles =
            iterWorld (width, height) (fun (x, y) ->
                let (fx, fy, bottom, right) = worldVars x y height width

                let rocks =
                    [ (2, 2)
                      (5, 5)
                      (7, 6)
                      (6, 6)
                      (6, 8)
                      (6, 6)
                      (7, 8)
                      (8, 7)
                      (8, 6) ]

                let observers = [ (3, 3); (3, 4); (6, 7) ]

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall fx fy
                | Wall (bottom, right) wallType -> createCollidableTile wallType fx fy
                | xy when (List.contains xy rocks) -> createRockOnGrass (Vector2(fx, fy)) time true
                | xy when (List.contains xy observers) ->
                    observerOnGrass (Vector2(fx, fy)) time (buildObserver Id) FacingLeft true
                | 7, 7 -> createEntityOn (Box {Items = []; IsOpen = true }) Grass (Vector2(fx, fy)) time true
                | 8, 8 -> createEntityOn (GoToLevelButton L3) Grass (Vector2(fx, fy)) time false

                | _ -> createNonCollidableTile FloorType.Grass fx fy)

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level4: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let tiles =
            iterWorld (width, height) (fun (x, y) ->
                let (fx, fy, bottom, right) = worldVars x y height width
                let obs = [ (2, 4); (2, 5); (6, 5); (6, 4); (6, 3) ]
                //TODO target just honors facing
                let targetUp = (coordsToIndex (x, y - 1) (width, height))
                let targetLeft = (coordsToIndex (x - 1, y) (width, height))

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall fx fy
                | Wall (bottom, right) wallType -> createCollidableTile wallType fx fy
                | xy when (List.contains xy obs) ->
                    observerOnGrass (Vector2(fx, fy)) time (observing Id targetUp ValueNone) FacingUp true
                | 2, 6
                | 2, 7
                | 2, 8
                | 6, 7
                | 6, 8
                | 6, 6 -> observerOnGrass (Vector2(fx, fy)) time (observing (Toggle true) targetUp ValueNone) FacingUp false
                | 7, 6
                | 8, 6 -> observerOnGrass (Vector2(fx, fy)) time (observing (Toggle true) targetLeft ValueNone) FacingLeft false

                | 2, 3 -> createButtonOnGrass (Vector2(fx, fy)) time false
                | 8, 8 -> createEntityOn (GoToLevelButton L4) Grass (Vector2(fx, fy)) time false


                | _ -> createNonCollidableTile FloorType.Grass fx fy)

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = [ (observerEntity (buildObserver Id)); (observerEntity (buildObserver Id)) ]
          Tiles = tiles
          Size = (width, height) }


let level5: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let tiles =
            iterWorld (width, height) (fun (x, y) ->
                let (fx, fy, bottom, right) = worldVars x y height width
                let obs = [ (2, 4); (2, 5); (6, 5); (6, 4); (6, 3) ]
                //TODO target just honors facing
                let targetUp = (coordsToIndex (x, y - 1) (width, height))
                let targetLeft = (coordsToIndex (x - 1, y) (width, height))

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall fx fy
                | Wall (bottom, right) wallType -> createCollidableTile wallType fx fy
                | xy when (List.contains xy obs) ->
                    observerOnGrass (Vector2(fx, fy)) time (observing Id targetUp ValueNone) FacingUp true
                | 2, 6
                | 2, 7
                | 2, 8
                | 6, 7
                | 6, 8
                | 6, 6 -> observerOnGrass (Vector2(fx, fy)) time (observing (Toggle true) targetUp ValueNone) FacingUp false
                | 7, 6
                | 8, 6 -> observerOnGrass (Vector2(fx, fy)) time (observing (Toggle true) targetLeft ValueNone) FacingLeft false

                | 2, 3 -> createTimerOnGrass (Vector2(fx, fy)) time false
                | 8, 8 -> createEntityOn (GoToLevelButton L1) Grass (Vector2(fx, fy)) time false
                | _ -> createNonCollidableTile FloorType.Grass fx fy)

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = [ (observerEntity (buildObserver Id)); (observerEntity (buildObserver Id)) ]
          Tiles = tiles
          Size = (width, height) }


let levelLookup (level: Level) : LevelBuilder =
    match level with
    | L1 -> level1
    | L2 -> level2
    | L3 -> level3
    | L4 -> level4
