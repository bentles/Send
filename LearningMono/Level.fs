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

let level_PlayerMoves: LevelBuilder =
    fun time ->
        let width = 7
        let height = 7

        let tiles =
            iterWorld (width, height) (fun (x, y) (_, _) (bottom, right) ->
                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall (x, y)
                | Wall (bottom, right) wallType -> createCollidableTile wallType (x, y)
                | 5, 4 -> createButtonOnGrass time false (x, y)
                | 5, 5 -> observerCannotPick time (observing GoToNextLevel) FacingUp (x, y)
                //some kind of goal
                | _ -> createNonCollidableTile FloorType.Grass (x, y))

        { PlayerStartsAtPos = Vector2(150f, 150f)
          PlayerStartsCarrying = []
          LevelText = "Press the arrow keys to move and Z to interact"
          Tiles = tiles
          Size = (width, height) }

let level_PlayerPickUp: LevelBuilder =
    fun time ->
        let width = 7
        let height = 7

        let tiles =
            iterWorld (width, height) (fun (x, y) (_, _) (bottom, right) ->

                match x, y with
                | Corner (bottom, right) _ -> createCollidableTile Wall (x, y)
                | Wall (bottom, right) wallType -> createCollidableTile wallType (x, y)
                | 1, 1 -> createButtonOnGrass time true (x, y)
                | 5, 5 -> observerCannotPick time (observing GoToNextLevel) FacingUp (x, y)

                | _ -> createNonCollidableTile FloorType.Grass (x, y))

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "Press X on an item to pick it up! Press C to place an item down. Z: Interact"
          Tiles = tiles
          Size = (width, height) }

let level_left: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wt = createCollidableTile FloorType.TopWall
        let bb = createButtonOnGrass time true
        let rr = createRockOnGrass time true

        let xu = observerCannotPick time (observing Id) FacingUp
        let xl = observerCannotPick time (observing Id) FacingLeft
        let xr = observerCannotPick time (observing Id) FacingRight
        let NL = observerCannotPick time (observing GoToNextLevel) FacingLeft


        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; ww ]
                  [ ww; bb; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; NL; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "X: Pick, C: Place, Z: Interact"
          Tiles = tiles
          Size = (width, height) }

let level_dang_rocks: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wt = createCollidableTile FloorType.TopWall
        let bb = createButtonOnGrass time true
        let rr = createRockOnGrass time true

        let NL = observerCannotPick time (observing GoToNextLevel) FacingLeft


        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; ww ]
                  [ ww; bb; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; rr; rr; ww ]
                  [ ww; __; __; rr; rr; rr; ww ]
                  [ ww; rr; rr; rr; rr; NL; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "You can pick up multiple items"
          Tiles = tiles
          Size = (width, height) }

let level_observers: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wt = createCollidableTile FloorType.TopWall
        let bb = createButtonOnGrass time true

        let xu = observerCannotPick time (observing Id) FacingUp
        let xl = observerCannotPick time (observing Id) FacingLeft
        let xr = observerCannotPick time (observing Id) FacingRight
        let NL = observerCannotPick time (observing GoToNextLevel) FacingUp


        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; bb; __; __; __; ww ]
                  [ ww; __; xu; __; xu; __; ww ]
                  [ ww; __; xu; __; NL; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "Observers re-transmit whatever signal they observe"
          Tiles = tiles
          Size = (width, height) }

let level_observers2: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wt = createCollidableTile FloorType.TopWall
        let bb = createButtonOnGrass time false

        let xu = observerCannotPick time (observing Id) FacingUp
        let xl = observerCanPick time (observing Id) FacingLeft
        let xr = observerCanPick time (observing Id) FacingRight
        let NL = observerCannotPick time (observing GoToNextLevel) FacingRight


        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; NL; __; __; bb; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; xr; xr; xr; xr; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "X: Pick, C: Place, Z: Interact"
          Tiles = tiles
          Size = (width, height) }

let level_observers3: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wt = createCollidableTile FloorType.TopWall
        let bb = createButtonOnGrass time false

        let xu = observerCannotPick time (observing Id) FacingUp
        let xl = observerCanPick time (observing Id) FacingLeft
        let xr = observerCanPick time (observing Id) FacingRight
        let NL = observerCannotPick time (observing GoToNextLevel) FacingLeft


        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; bb; xr; xr; NL; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "Hold shift and press arrow keys to change the direction of a placed object"
          Tiles = tiles
          Size = (width, height) }

let level_observers4: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wt = createCollidableTile FloorType.TopWall
        let bb = createButtonOnGrass time false

        let xu = observerCannotPick time (observing Id) FacingUp
        let xl = observerCanPick time (observing Id) FacingLeft
        let xr = observerCanPick time (observing Id) FacingRight
        let NL = observerCannotPick time (observing GoToNextLevel) FacingLeft


        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; NL; ww ]
                  [ ww; __; bb; __; __; __; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; xr; xr; xr; xr; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "Hold shift and press arrow keys to change the placement direction"
          Tiles = tiles
          Size = (width, height) }

let level_observers5: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wt = createCollidableTile FloorType.TopWall
        let bb = createButtonOnGrass time false

        let xu = observerCannotPick time (observing Id) FacingUp
        let xl = observerCanPick time (observing Id) FacingLeft
        let xr = observerCanPick time (observing Id) FacingRight
        let NL = observerCannotPick time (observing GoToNextLevel) FacingUp


        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; NL; ww ]
                  [ ww; __; __; __; __; __; ww ]
                  [ ww; __; bb; __; __; __; ww ]
                  [ ww; __; xr; xr; xr; xr; ww ]
                  [ ww; xr; xr; xr; xr; xr; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "Press shift+arrow keys to change the direction an object is facing"
          Tiles = tiles
          Size = (width, height) }

let level_box1: LevelBuilder =
    fun time ->
        let g = createNonCollidableTile FloorType.Grass
        let w = createCollidableTile Wall
        let l = createCollidableTile FloorType.LeftWall
        let r = createCollidableTile FloorType.RightWall
        let b = createCollidableTile FloorType.BottomWall
        let t = createCollidableTile FloorType.TopWall
        let N = observerCannotPick time (observing GoToNextLevel) FacingDown

        let x =
            createEntityOn
                (Box
                    { Items = [ Rock; Rock; (buildSubject Button) ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ w; t; t; t; t; t; w ]
                  [ l; g; N; g; g; g; r ]
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; g; x; g; r ]
                  [ w; b; b; b; b; b; w ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "Boxes store items. Try using Z (interact), X (pick up) and C (place) on a box"
          Tiles = tiles
          Size = (width, height) }


let level_box2: LevelBuilder =
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
                          buildObserver GoToNextLevel
                          Box
                              { Items =
                                  [ Box { Items = []; IsOpen = true }
                                    Box
                                        { Items = [ Rock; Rock; Rock; (buildSubject Button) ]
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
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; x; g; g; r ]
                  [ l; g; g; g; g; g; r ]
                  [ l; g; g; g; g; g; r ]
                  [ w; b; b; b; b; b; w ] ]

        { PlayerStartsAtPos = (Vector2(200f, 100f))
          PlayerStartsCarrying = []
          LevelText = "Boxes store items. Try using Z (interact), X (pick up) and C (place) on a box"
          Tiles = tiles
          Size = (width, height) }

let levelPlaceDirections: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let _f = createNonCollidableTile FloorType.Floor
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let ir = observerCanPick time (observing Id) FacingRight
        let il = observerCannotPick time (observing Id) FacingUp
        let bb = createButtonOnGrass time false
        let xx = observerCannotPick time (observing GoToNextLevel) FacingUp

        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; wt; wt; wt; ww ]
                  [ wl; __; __; __; __; __; __; __; __; wr ]
                  [ wl; __; bb; __; __; __; __; __; __; wr ]
                  [ wl; __; __; __; __; __; __; __; __; wr ]
                  [ wl; __; __; ir; il; il; il; il; il; wr ]
                  [ wl; __; __; __; il; _f; _f; _f; _f; wr ]
                  [ wl; __; __; __; il; _f; _f; _f; xx; wr ]
                  [ ww; wb; wb; wb; wb; wb; wb; wb; wb; ww ] ]

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          LevelText =
            "Shift + arrow keys will change your placement direction. Try placing items in different orientations!"
          Size = (width, height) }

let level_toggles: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let ir = observerOnGrass time (observing Id) FacingRight true
        let tu = observerOnGrass time (observing (Toggle true)) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true)) FacingLeft false
        let bb = createButtonOnGrass time false
        let xx = createEntityOn (buildObserver GoToNextLevel) Grass time true
        let bx =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id) ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; wt; wt; wt; ww ]
                  [ wl; __; __; __; __; __; __; __; bx; wr ]
                  [ wl; __; bb; __; __; __; __; __; __; wr ]
                  [ wl; __; tu; __; __; __; __; __; __; wr ]
                  [ wl; __; tu; __; __; tl; tl; tl; tl; wr ]
                  [ wl; __; tu; __; __; __; tu; __; __; wr ]
                  [ wl; __; tu; __; __; __; tu; __; xx; wr ]
                  [ ww; wb; wb; wb; wb; wb; wb; wb; wb; ww ] ]

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          LevelText = ""
          Size = (width, height) }

let level_map1: LevelBuilder =
    fun time ->
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let iu = observerOnGrass time (observing Id) FacingUp true
        let il = observerOnGrass time (observing Id) FacingLeft true
        let tu = observerOnGrass time (observing (Toggle true)) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true)) FacingLeft false

        let fl = observerOnGrass time (observing (Filter(observing Id))) FacingLeft false

        let mu = observerOnGrass time (observing (Map Unit)) FacingUp true
        let bb = createButtonOnGrass time false
        let xx = createEntityOn ((observing GoToNextLevel)) Grass time false

        let bx =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id) ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; wt; wt; wt; ww ]
                  [ ww; __; __; __; __; __; bx; __; __; ww ]
                  [ ww; __; bb; __; __; __; __; __; __; ww ]
                  [ ww; __; iu; __; __; __; __; __; __; ww ]
                  [ ww; __; iu; __; __; __; tl; tl; tl; ww ]
                  [ ww; __; mu; __; __; __; tu; __; __; ww ]
                  [ ww; __; iu; il; il; __; tu; __; xx; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          LevelText = "The Map observer changes the signal to whatever object you place inside it"
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
        let ir = observerOnGrass time (observing Id) FacingRight true
        let me = observerOnGrass time (observing Merge) FacingDown false
        let tu = observerOnGrass time (observing (Toggle true)) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true)) FacingLeft false
        let td = observerOnGrass time (observing (Toggle true)) FacingDown false
        let tr = observerOnGrass time (observing (Toggle true)) FacingRight false
        let Tu = observerOnGrass time (observing (Toggle false)) FacingUp false
        let Td = observerOnGrass time (observing (Toggle false)) FacingDown false
        let Tl = observerOnGrass time (observing (Toggle false)) FacingLeft false

        let fl = observerOnGrass time (observing (Filter Rock)) FacingLeft false

        let fu = observerOnGrass time (observing (Filter(observing Id))) FacingLeft false

        let ma = observerOnGrass time (observing (Map Unit)) FacingLeft true
        let bb = createButtonOnGrass time true
        let xx = createEntityOn ((observing Id)) Grass time false

        let bx =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id) ]
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
          LevelText = ""
          Size = (width, height) }

let levelSandBox: LevelBuilder =
    fun time ->
        let _v = createNonCollidableTile FloorType.Void
        let __ = createNonCollidableTile FloorType.Grass
        let ww = createCollidableTile Wall
        let wl = createCollidableTile FloorType.LeftWall
        let rr = createRockOnGrass time true
        let wr = createCollidableTile FloorType.RightWall
        let wb = createCollidableTile FloorType.BottomWall
        let wt = createCollidableTile FloorType.TopWall
        let ir = observerOnGrass time (observing Id) FacingRight false
        let il = observerOnGrass time (observing Id) FacingLeft false
        let iu = observerOnGrass time (observing Id) FacingUp false
        let id = observerOnGrass time (observing Id) FacingDown false
        let me = observerOnGrass time (observing Merge) FacingDown false
        let tu = observerOnGrass time (observing (Toggle true)) FacingUp false
        let tl = observerOnGrass time (observing (Toggle true)) FacingLeft false
        let td = observerOnGrass time (observing (Toggle true)) FacingDown false
        let tr = observerOnGrass time (observing (Toggle true)) FacingRight false
        let Tu = observerOnGrass time (observing (Toggle false)) FacingUp false
        let Td = observerOnGrass time (observing (Toggle false)) FacingDown false
        let Tl = observerOnGrass time (observing (Toggle false)) FacingLeft false

        let fl = observerOnGrass time (observing (Filter Rock)) FacingLeft false

        let fu = observerOnGrass time (observing (Filter(observing Id))) FacingLeft false

        let ma = observerOnGrass time (observing (Map Unit)) FacingLeft true
        let bb = createButtonOnGrass time true
        let xx = createEntityOn ((observing Id)) Grass time false

        let b0 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing (Toggle true))
                          (observing (Toggle true))
                          (observing (Toggle true))
                          (observing (Toggle true))
                          (observing (Toggle true))
                          (observing (Toggle false))
                          (observing (Toggle false))
                          (observing (Toggle false))
                          (observing (Toggle false))
                          (observing (Toggle false)) ]
                      IsOpen = false })
                Grass
                time
                true

        let b1 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id)
                          (observing Id) ]
                      IsOpen = false })
                Grass
                time
                true

        let b2 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing Merge)
                          (observing Merge)
                          (observing Merge)
                          (observing Merge)
                          (observing Merge)
                          (observing Merge)
                          (observing Merge)
                          (observing Merge) ]
                      IsOpen = false })
                Grass
                time
                true

        let b3 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing (Map Unit))
                          (observing (Map Unit))
                          (observing (Map Unit))
                          (observing (Map Unit))
                          (observing (Map Unit))
                          (observing (Map Unit))
                          (observing (Map Unit))
                          (observing (Map Unit)) ]
                      IsOpen = false })
                Grass
                time
                true

        let b4 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing (Filter Unit))
                          (observing (Filter Unit))
                          (observing (Filter Unit))
                          (observing (Filter Unit))
                          (observing (Filter Unit))
                          (observing (Filter Unit))
                          (observing (Filter Unit))
                          (observing (Filter Unit)) ]
                      IsOpen = false })
                Grass
                time
                true

        let b5 =
            createEntityOn
                (Box
                    { Items =
                        [ (observing (Compare))
                          (observing (Compare))
                          (observing (Compare))
                          (observing (Compare))
                          (observing (Compare))
                          (observing (Compare))
                          (observing (Compare))
                          (observing (Compare)) ]
                      IsOpen = false })
                Grass
                time
                true

        let tiles, width, height =
            worldFromTemplate
                [ [ ww; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; ww ]
                  [ ww; __; __; __; __; __; __; __; __; __; __; __; __; ww ]
                  [ ww; bb; __; __; b0; b1; b2; b3; b4; b5; __; __; __; ww ]
                  [ ww; bb; __; __; __; __; __; __; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; __; __; __; __; __; ww; wt; wt ]
                  [ ww; __; __; __; __; __; __; __; __; __; __; ww; _v; _v ]
                  [ ww; rr; __; __; __; __; __; __; __; __; __; ww; _v; _v ]
                  [ ww; __; __; __; __; __; __; __; __; __; __; ww; _v; _v ]
                  [ ww; __; __; __; __; __; __; __; __; __; __; wt; wt; ww ]
                  [ ww; __; __; __; __; __; __; __; __; __; __; __; __; ww ]
                  [ ww; __; __; __; __; __; __; __; __; __; __; __; __; ww ]
                  [ wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt; wt ] ]

        { PlayerStartsAtPos = Vector2(150f, 100f)
          PlayerStartsCarrying = []
          LevelText = "This is the sandbox level. Go nuts!"
          Tiles = tiles
          Size = (width, height) }

let levels: LevelBuilder[] =
    [|     
       level_PlayerMoves
       level_PlayerPickUp
       level_left
       level_dang_rocks
       level_observers
       level_observers2
       level_observers3
       level_observers4
       level_observers5
       level_box1
       level_box2
       level_toggles
       level_map1
       level7
       levelSandBox |]
