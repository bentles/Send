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
      Collider: AABB option
      Entity: Entity.Model option }

let defaultTile =
    { FloorType = FloorType.Empty
      Collider = None
      Entity = None }

type Level =
    { PlayerStartsAtPos: Vector2
      PlayerStartsCarrying: Entity.Model list
      Tiles: PersistentVector<Tile>
      Size: (int * int) }

type LevelBuilder = int64 -> Level

// helpers
let createCollidableTile t x y =
    { defaultTile with
        FloorType = t
        Collider = Some(createColliderFromCoords x y half) }

let createNonCollidableTile t = { defaultTile with FloorType = t }

let createTimerOnGrass (coords: Vector2) time =
    let pos = coordsToVector coords.X coords.Y half

    let subject =
        Entity.Subject
            { Type = Entity.Timer([ Rock; buildObserver Id; buildObserver (Map Rock); rockTimer ], 60)
              TicksSinceEmit = 0
              GenerationNumber = 0
              ToEmit = Nothing }

    { defaultTile with
        FloorType = FloorType.Grass
        Entity = Some(Entity.init subject pos time FacingRight) }

let createObserverOnGrass (coords: Vector2) time observer : Tile =
    let pos = coordsToVector coords.X coords.Y half

    { defaultTile with
        FloorType = FloorType.Grass
        Entity = Some(Entity.init observer pos time FacingRight) }

let iterWorld (width: int, height: int) (func: (int * int) -> Tile) : PersistentVector<Tile> =
    seq {
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                func (x, y)
    }
    |> PersistentVector.ofSeq

let level1: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let grassTile = createNonCollidableTile FloorType.Grass

        let tiles =
            iterWorld (width, height) (fun (x, y) ->
                let bottom = height - 1
                let right = width - 1 
                match x, y with
                | x, y when y = 0 && x = 0 -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom && x = right -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = 0 && x = right -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom && x = 0 -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom -> createCollidableTile BottomWall (float32 x) (float32 y)
                | x, y when y = 0 -> createCollidableTile TopWall (float32 x) (float32 y)
                | x, y when x = 0 -> createCollidableTile LeftWall (float32 x) (float32 y)
                | x, y when x = right -> createCollidableTile RightWall (float32 x) (float32 y)

                | 2, 2 -> createTimerOnGrass (Vector2(2f, 2f)) time
                | _ -> grassTile)

        { PlayerStartsAtPos = Vector2(150f, 150f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level2: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let grassTile = createNonCollidableTile FloorType.Grass

        let tiles =
            iterWorld (width, height) (fun (x, y) ->
                let bottom = height - 1
                let right = width - 1 

                match x, y with
                | x, y when y = 0 && x = 0 -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom && x = right -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = 0 && x = right -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom && x = 0 -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom -> createCollidableTile BottomWall (float32 x) (float32 y)
                | x, y when y = 0 -> createCollidableTile TopWall (float32 x) (float32 y)
                | x, y when x = 0 -> createCollidableTile LeftWall (float32 x) (float32 y)
                | x, y when x = right -> createCollidableTile RightWall (float32 x) (float32 y)
                | _ -> grassTile)

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }
