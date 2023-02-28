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
        Grass
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
    createSubjectOnGrass (Entity.Button Unit) time pickedUp coords

let observerEntity observer =
    Entity.init observer Vector2.Zero 0 FacingLeft true

let observerOnGrass time observer facing canPickup (coords: Coords) : Tile =
    let pos = CoordsFToOffsetVector (toCoordsF coords) half

    { defaultTile with
        Coords = coords
        FloorType = FloorType.Grass
        Entity = ValueSome(Entity.init observer pos time facing canPickup) }

let worldVars x y height width =
    (float32 x, float32 y, height - 1, width - 1)

let iterWorld (width: int, height: int) (func: (int * int) -> (float32 * float32) -> (int * int) -> Tile) : Tiles =
    seq {
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                let (fx, fy, bottom, right) = worldVars x y height width
                func (x, y) (fx, fy) (bottom, right)
    }
    |> PersistentVector.ofSeq


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
            iterWorld (width, height) (fun (x, y) (_, _) (bottom, right) ->
                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall (x, y)
                | Wall (bottom, right) wallType -> createCollidableTile wallType (x, y)

                | 5, 5 -> createEntityOn (GoToLevelButton L2) Grass time false (x, y)
                //some kind of goal
                | _ -> createNonCollidableTile FloorType.Grass (x, y))

        { PlayerStartsAtPos = Vector2(150f, 150f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level2: LevelBuilder =
    fun time ->
        let width = 7
        let height = 7

        let tiles =
            iterWorld (width, height) (fun (x, y) (_, _) (bottom, right) ->
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
                | Corner (bottom, right) _ -> createCollidableTile Wall (x, y)
                | Wall (bottom, right) wallType -> createCollidableTile wallType (x, y)
                | xy when (List.contains xy rocks) -> createRockOnGrass time true (x, y)

                | 5, 5 -> createEntityOn (GoToLevelButton L3) Grass time false (x, y)

                | _ -> createNonCollidableTile FloorType.Grass (x, y))

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
                                  [ Box { Items = []; IsOpen = true }
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
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let ir = observerOnGrass time (observing Id true false) FacingRight true
        let tu = observerOnGrass time (observing (Toggle true) true false) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true) true false) FacingLeft false
        let bb = createButtonOnGrass time false
        let xx = createEntityOn (GoToLevelButton L5) Grass time false

        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; wt; wt; wt; ww ]
                  [ wl; __; __; __; __; __; __; __; __; wr ]
                  [ wl; __; bb; __; __; __; __; __; __; wr ]
                  [ wl; __; tu; __; __; __; __; __; __; wr ]
                  [ wl; __; tu; ir; tl; tl; tl; tl; tl; wr ]
                  [ wl; __; tu; __; __; __; tu; __; __; wr ]
                  [ wl; __; tu; __; __; __; tu; __; xx; wr ]
                  [ ww; wb; wb; wb; wb; wb; wb; wb; wb; ww ] ]

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level5: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let ir = observerOnGrass time (observing Id true false) FacingRight true
        let tu = observerOnGrass time (observing (Toggle true) true false) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true) true false) FacingLeft false
        let bb = createButtonOnGrass time false
        let xx = createEntityOn (GoToLevelButton L5) Grass time false

        let bx =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false) ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; wt; wt; wt; ww ]
                  [ wl; __; __; __; __; __; bx; __; __; wr ]
                  [ wl; __; bb; __; __; __; __; __; __; wr ]
                  [ wl; __; __; __; __; __; __; __; __; wr ]
                  [ wl; __; __; ir; __; __; tl; tl; tl; wr ]
                  [ wl; __; __; __; __; __; tu; __; __; wr ]
                  [ wl; __; __; __; __; __; tu; __; xx; wr ]
                  [ ww; wb; wb; wb; wb; wb; wb; wb; wb; ww ] ]

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level6: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let ir = observerOnGrass time (observing Id true false) FacingRight true
        let tu = observerOnGrass time (observing (Toggle true) true false) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true) true false) FacingLeft false

        let fl =
            observerOnGrass time (observing (Filter(observing Id true false)) true false) FacingLeft false

        let ml = observerOnGrass time (observing (Map Unit) true false) FacingLeft true
        let bb = createButtonOnGrass time false
        let xx = createEntityOn (GoToLevelButton L6) Grass time false

        let bx =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false) ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; wt; wt; wt; ww ]
                  [ ww; __; __; __; __; __; bx; __; __; ww ]
                  [ ww; __; bb; __; __; __; __; ml; __; ww ]
                  [ ww; __; __; __; __; __; __; __; __; ww ]
                  [ ww; __; __; ir; __; __; fl; tl; tl; ww ]
                  [ ww; __; __; __; __; __; tu; __; __; ww ]
                  [ ww; __; __; __; __; __; tu; __; xx; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level7: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let rr = createRockOnGrass time true
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let ir = observerOnGrass time (observing Id true false) FacingRight true
        let me = observerOnGrass time (observing Merge true true) FacingDown false
        let tu = observerOnGrass time (observing (Toggle true) true false) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true) true false) FacingLeft false
        let td = observerOnGrass time (observing (Toggle true) true false) FacingDown false
        let tr = observerOnGrass time (observing (Toggle true) true false) FacingRight false
        let Tu = observerOnGrass time (observing (Toggle false) true false) FacingUp false
        let Td = observerOnGrass time (observing (Toggle false) true false) FacingDown false
        let Tl = observerOnGrass time (observing (Toggle false) true false) FacingLeft false

        let fl = observerOnGrass time (observing (Filter Rock) true false) FacingLeft false

        let fu =
            observerOnGrass time (observing (Filter(observing Id true false)) true false) FacingLeft false

        let ma = observerOnGrass time (observing (Map Unit) true false) FacingLeft true
        let bb = createButtonOnGrass time true
        let xx = createEntityOn (GoToLevelButton L8) Grass time false

        let bx =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false) ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; wt; wt; wt; ww ]
                  [ wl; __; __; __; td; me; Tl; __; __; wr ]
                  [ wl; bb; __; __; td; __; Tu; __; __; wr ]
                  [ wl; __; __; __; td; __; Tu; __; __; wr ]
                  [ wl; rr; __; __; td; __; Tu; __; __; wr ]
                  [ wl; __; __; __; td; __; Tu; __; __; wr ]
                  [ wl; __; __; __; td; __; Tu; __; __; wr ]
                  [ wl; __; __; __; tl; wb; Tu; __; xx; wr ]
                  [ ww; wb; wb; wb; wb; ww; wb; wb; wb; ww ] ]

        { PlayerStartsAtPos = Vector2(150f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let sandBox: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let rr = createRockOnGrass time true
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let ir = observerOnGrass time (observing Id true false) FacingRight false
        let il = observerOnGrass time (observing Id true false) FacingLeft false
        let iu = observerOnGrass time (observing Id true false) FacingUp false
        let id = observerOnGrass time (observing Id true false) FacingDown false
        let me = observerOnGrass time (observing Merge true true) FacingDown false
        let tu = observerOnGrass time (observing (Toggle true) true false) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true) true false) FacingLeft false
        let td = observerOnGrass time (observing (Toggle true) true false) FacingDown false
        let tr = observerOnGrass time (observing (Toggle true) true false) FacingRight false
        let Tu = observerOnGrass time (observing (Toggle false) true false) FacingUp false
        let Td = observerOnGrass time (observing (Toggle false) true false) FacingDown false
        let Tl = observerOnGrass time (observing (Toggle false) true false) FacingLeft false

        let fl = observerOnGrass time (observing (Filter Rock) true false) FacingLeft false

        let fu =
            observerOnGrass time (observing (Filter(observing Id true false)) true false) FacingLeft false

        let ma = observerOnGrass time (observing (Map Unit) true false) FacingLeft true
        let bb = createButtonOnGrass time true
        let xx = createEntityOn (GoToLevelButton L7) Grass time false

        let b0 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing (Toggle true) true false)
                          (observing (Toggle true) true false)
                          (observing (Toggle true) true false)
                          (observing (Toggle true) true false)
                          (observing (Toggle true) true false)
                          (observing (Toggle false) true false)
                          (observing (Toggle false) true false)
                          (observing (Toggle false) true false)
                          (observing (Toggle false) true false)
                          (observing (Toggle false) true false) ]
                      IsOpen = false })
                Grass
                time
                true

        let b1 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false)
                          (observing Id true false) ]
                      IsOpen = false })
                Grass
                time
                true

        let b2 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Merge true true)
                          (observing Merge true true)
                          (observing Merge true true)
                          (observing Merge true true)
                          (observing Merge true true)
                          (observing Merge true true)
                          (observing Merge true true)
                          (observing Merge true true) ]
                      IsOpen = false })
                Grass
                time
                true

        let b3 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing (Map Unit) true false)
                          (observing (Map Unit) true false)
                          (observing (Map Unit) true false)
                          (observing (Map Unit) true false)
                          (observing (Map Unit) true false)
                          (observing (Map Unit) true false)
                          (observing (Map Unit) true false)
                          (observing (Map Unit) true false) ]
                      IsOpen = false })
                Grass
                time
                true

        let b4 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing (Filter Unit) true false)
                          (observing (Filter Unit) true false)
                          (observing (Filter Unit) true false)
                          (observing (Filter Unit) true false)
                          (observing (Filter Unit) true false)
                          (observing (Filter Unit) true false)
                          (observing (Filter Unit) true false)
                          (observing (Filter Unit) true false) ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ id; il; il; il; il; il; il; il; il; il; il; il; il; il ]
                  [ id; __; __; __; __; __; __; __; __; __; __; __; ww; iu ]
                  [ id; bb; __; __; b0; b1; b2; b3; b4; __; __; __; wr; iu ]
                  [ id; bb; __; __; __; __; __; __; __; __; __; __; wr; iu ]
                  [ id; __; __; __; __; __; __; __; __; __; __; __; wr; iu ]
                  [ id; __; __; __; __; __; __; __; __; __; __; __; wr; iu ]
                  [ id; rr; __; __; __; __; __; __; __; __; __; __; wr; iu ]
                  [ id; __; __; __; __; __; __; __; __; __; __; __; wr; iu ]
                  [ id; __; __; __; __; __; __; bb; __; __; __; __; wr; iu ]
                  [ id; __; __; __; __; __; __; iu; __; __; __; __; wr; iu ]
                  [ id; __; __; __; __; __; __; iu; __; __; __; __; ir; iu ]
                  [ ir; ir; ir; ir; ir; ir; ir; iu; wb; wb; wb; wb; wb; ww ] ]

        { PlayerStartsAtPos = Vector2(150f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }



let levelLookup (level: Level) : LevelBuilder =
    match level with
    | L1 -> level1
    | L2 -> level2
    | L3 -> level3
    | L4 -> level4
    | L5 -> level5
    | L6 -> level6
    | L7 -> level7
    | L8 -> sandBox
