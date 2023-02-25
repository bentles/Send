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
let createCollidableTile t (coords: Vector2) =
    { defaultTile with
        Coords = coords
        FloorType = t
        Collider = ValueSome(createColliderFromCoords coords.X coords.Y half) }

let createNonCollidableTile t (coords: Vector2) =
    { defaultTile with
        FloorType = t
        Coords = coords }

let createEntityOn (entityType: EntityType) (floor: FloorType) (time: int64) (canBePickedUp: bool) (coords: Vector2) =
    let pos = coordsToVector coords.X coords.Y half

    { defaultTile with
        Coords = coords
        FloorType = floor
        Entity = ValueSome(Entity.init entityType pos time FacingRight canBePickedUp) }

let createRockOnGrass time (canBePickedUp: bool) (coords: Vector2) =
    createEntityOn Rock Grass time canBePickedUp coords

let createSubjectOnGrass (subjectType: SubjectType) time pickedUp (coords: Vector2) =
    createEntityOn
        (Entity.Subject
            { Type = subjectType
              TicksSinceEmit = 0
              GenerationNumber = 0
              ToEmit = Nothing })
        Grass
        time
        pickedUp
        coords

let createTimerOnGrass time pickedUp (coords: Vector2) =
    createSubjectOnGrass
        (Entity.Timer(
            { Items = [ Rock; buildObserver Id; buildObserver (Map Rock); rockTimer ]
              IsOpen = false },
            60
        ))
        time
        pickedUp
        coords

let createButtonOnGrass time pickedUp (coords: Vector2) =
    createSubjectOnGrass (Entity.Button Rock) time pickedUp coords

let observerEntity observer =
    Entity.init observer Vector2.Zero 0 FacingLeft true

let observerOnGrass time observer facing canPickup (coords: Vector2) : Tile =
    let pos = coordsToVector coords.X coords.Y half

    { defaultTile with
        Coords = coords
        FloorType = FloorType.Grass
        Entity = ValueSome(Entity.init observer pos time facing canPickup) }

let worldVars x y height width =
    (float32 x, float32 y, height - 1, width - 1)

let iterWorld
    (width: int, height: int)
    (func: (int * int) -> (float32 * float32) -> (int * int) -> Tile)
    : PersistentVector<Tile> =
    seq {
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                let (fx, fy, bottom, right) = worldVars x y height width
                func (x, y) (fx, fy) (bottom, right)
    }
    |> PersistentVector.ofSeq


let worldFromTemplate (template: List<List<Vector2 -> Tile>>) =
    seq {
        for y, row in List.indexed (template) do
            for x, tileBuilder in List.indexed (row) do
                let loc = Vector2(float32 x, float32 y)
                tileBuilder loc
    }
    |> PersistentVector.ofSeq,
    template.Head.Length,
    template.Length

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



let level1: LevelBuilder =
    fun time ->
        let width = 7
        let height = 7

        let tiles =
            iterWorld (width, height) (fun (x, y) (fx, fy) (bottom, right) ->
                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall (Vector2(fx, fy))
                | Wall (bottom, right) wallType -> createCollidableTile wallType (Vector2(fx, fy))

                | 5, 5 -> createEntityOn (GoToLevelButton L2) Grass time false (Vector2(fx, fy))
                //some kind of goal
                | _ -> createNonCollidableTile FloorType.Grass (Vector2(fx, fy)))

        { PlayerStartsAtPos = Vector2(150f, 150f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level2: LevelBuilder =
    fun time ->
        let width = 7
        let height = 7

        let tiles =
            iterWorld (width, height) (fun (x, y) (fx, fy) (bottom, right) ->
                let rocks =
                    [ (2, 2)
                      (3, 5)
                      (3, 3)
                      (4, 3)
                      (5, 3)
                      (3, 4)
                      (3, 5)
                      (4, 5)
                      (4, 4)
                      (5, 4)
                      (5, 2) ]

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall (Vector2(fx, fy))
                | Wall (bottom, right) wallType -> createCollidableTile wallType (Vector2(fx, fy))
                | xy when (List.contains xy rocks) -> createRockOnGrass time true (Vector2(fx, fy))

                | 5, 5 -> createEntityOn (GoToLevelButton L3) Grass time false (Vector2(fx, fy))

                | _ -> createNonCollidableTile FloorType.Grass (Vector2(fx, fy)))

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }


let level3: LevelBuilder =
    fun time ->
        let g = createNonCollidableTile FloorType.Grass
        let w = createCollidableTile Wall
        let l = createCollidableTile FloorType.LeftWall
        let r = createCollidableTile FloorType.RightWall
        let b = createCollidableTile FloorType.BottomWall
        let t = createCollidableTile FloorType.TopWall

        let x =
            createEntityOn
                (Box
                    { Items =
                        [ Rock
                          Rock
                          Box
                              { Items =
                                  [ Box { Items = []; IsOpen = true}
                                    Box
                                        { Items = [ Rock; Rock; Rock; (GoToLevelButton L4) ]
                                          IsOpen = false } ]
                                IsOpen = false } ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ w; t; t; t; t; t; w ]
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; g; b; b; r ]
                  [ l; g; g; g; t; t; r ]
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; g; x; g; r ]
                  [ w; b; b; b; b; b; w ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level4: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let tiles =
            iterWorld (width, height) (fun (x, y) (fx, fy) (bottom, right) ->
                let obs = [ (2, 4); (2, 5); (6, 5); (6, 4); (6, 3) ]
                //TODO target just honors facing
                let targetUp = (coordsToIndex (x, y - 1) (width, height))
                let targetLeft = (coordsToIndex (x - 1, y) (width, height))

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall (Vector2(fx, fy))
                | Wall (bottom, right) wallType -> createCollidableTile wallType (Vector2(fx, fy))
                | xy when (List.contains xy obs) ->
                    observerOnGrass time (observing Id targetUp ValueNone) FacingUp true (Vector2(fx, fy))
                | 2, 6
                | 2, 7
                | 2, 8
                | 6, 7
                | 6, 8
                | 6, 6 ->
                    observerOnGrass time (observing (Toggle true) targetUp ValueNone) FacingUp false (Vector2(fx, fy))
                | 7, 6
                | 8, 6 ->
                    observerOnGrass
                        time
                        (observing (Toggle true) targetLeft ValueNone)
                        FacingLeft
                        false
                        (Vector2(fx, fy))

                | 2, 3 -> createButtonOnGrass time false (Vector2(fx, fy))
                | 8, 8 -> createEntityOn (GoToLevelButton L5) Grass time false (Vector2(fx, fy))
                | _ -> createNonCollidableTile FloorType.Grass (Vector2(fx, fy)))

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = [ (observerEntity (buildObserver Id)); (observerEntity (buildObserver Id)) ]
          Tiles = tiles
          Size = (width, height) }


let level5: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let tiles =
            iterWorld (width, height) (fun (x, y) (fx, fy) (bottom, right) ->
                let obs = [ (2, 4); (2, 5); (6, 5); (6, 4); (6, 3) ]
                //TODO target just honors facing
                let targetUp = (coordsToIndex (x, y - 1) (width, height))
                let targetLeft = (coordsToIndex (x - 1, y) (width, height))

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall (Vector2(fx, fy))
                | Wall (bottom, right) wallType -> createCollidableTile wallType (Vector2(fx, fy))
                | xy when (List.contains xy obs) ->
                    observerOnGrass time (observing Id targetUp ValueNone) FacingUp true (Vector2(fx, fy))
                | 2, 6
                | 2, 7
                | 2, 8
                | 6, 7
                | 6, 8
                | 6, 6 ->
                    observerOnGrass time (observing (Toggle true) targetUp ValueNone) FacingUp false (Vector2(fx, fy))
                | 7, 6
                | 8, 6 ->
                    observerOnGrass
                        time
                        (observing (Toggle true) targetLeft ValueNone)
                        FacingLeft
                        false
                        (Vector2(fx, fy))

                | 2, 3 -> createTimerOnGrass time false (Vector2(fx, fy))
                | 8, 8 -> createEntityOn (GoToLevelButton L1) Grass time false (Vector2(fx, fy))
                | _ -> createNonCollidableTile FloorType.Grass (Vector2(fx, fy)))

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
    | L5 -> level5
