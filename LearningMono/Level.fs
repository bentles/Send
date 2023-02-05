﻿module Level

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

type LevelData =
    { PlayerStartsAtPos: Vector2
      PlayerStartsCarrying: Entity.Model list
      Tiles: PersistentVector<Tile>
      Size: (int * int) }

type LevelBuilder = int64 -> LevelData

// helpers
let createCollidableTile t x y =
    { defaultTile with
        FloorType = t
        Collider = Some(createColliderFromCoords x y half) }

let createNonCollidableTile t = { defaultTile with FloorType = t }

let createEntityOnGrass (entityType: EntityType) (coords: Vector2) time (canBePickedUp: bool) =
    let pos = coordsToVector coords.X coords.Y half

    { defaultTile with
        FloorType = FloorType.Grass
        Entity = Some(Entity.init entityType pos time FacingRight canBePickedUp) }

let createRockOnGrass (coords: Vector2) time = createEntityOnGrass Rock coords time

let createTimerOnGrass (coords: Vector2) time =
    createEntityOnGrass
        (Entity.Subject
            { Type = Entity.Timer([ Rock; buildObserver Id; buildObserver (Map Rock); rockTimer ], 60)
              TicksSinceEmit = 0
              GenerationNumber = 0
              ToEmit = Nothing })
        coords
        time

let createObserverOnGrass (coords: Vector2) time observer : Tile =
    let pos = coordsToVector coords.X coords.Y half

    { defaultTile with
        FloorType = FloorType.Grass
        Entity = Some(Entity.init observer pos time FacingRight true) }

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

                | 7, 7 -> createEntityOnGrass (GoToLevelButton L2) (Vector2(7f, 7f)) time false
                //some kind of goal
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

                let rocks =
                    [ (2, 2); (5, 5); (7, 6); (6, 6); (6, 8);  (6, 6); (7, 8); (7, 7); (8, 7); (8, 6) ]

                let observers = [ (3, 3); (3, 4); (6, 7);]

                match x, y with
                | x, y when y = 0 && x = 0 -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom && x = right -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = 0 && x = right -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom && x = 0 -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom -> createCollidableTile BottomWall (float32 x) (float32 y)
                | x, y when y = 0 -> createCollidableTile TopWall (float32 x) (float32 y)
                | x, y when x = 0 -> createCollidableTile LeftWall (float32 x) (float32 y)
                | x, y when x = right -> createCollidableTile RightWall (float32 x) (float32 y)

                | x, y as xy when (List.contains xy rocks) ->
                    createRockOnGrass (Vector2(float32 x, float32 y)) time true
                | x, y as xy when (List.contains xy observers) ->
                    createObserverOnGrass (Vector2(float32 x, float32 y)) time (buildObserver Id)
                | 8, 8 -> createEntityOnGrass (GoToLevelButton L3) (Vector2(8f, 8f)) time false


                | _ -> grassTile)

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }

let level3: LevelBuilder =
    fun time ->
        let width = 10
        let height = 10

        let grassTile = createNonCollidableTile FloorType.Grass

        let tiles =
            iterWorld (width, height) (fun (x, y) ->
                let bottom = height - 1
                let right = width - 1

                let rocks = [ (2, 2); (3, 3); (5, 5) ]

                match x, y with
                | x, y when y = 0 && x = 0 -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom && x = right -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = 0 && x = right -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom && x = 0 -> createCollidableTile Wall (float32 x) (float32 y)
                | x, y when y = bottom -> createCollidableTile BottomWall (float32 x) (float32 y)
                | x, y when y = 0 -> createCollidableTile TopWall (float32 x) (float32 y)
                | x, y when x = 0 -> createCollidableTile LeftWall (float32 x) (float32 y)
                | x, y when x = right -> createCollidableTile RightWall (float32 x) (float32 y)

                | x, y as xy when (List.contains xy rocks) ->
                    createRockOnGrass (Vector2(float32 x, float32 y)) time true
                | 8, 8 -> createEntityOnGrass (GoToLevelButton L3) (Vector2(8f, 8f)) time false


                | _ -> grassTile)

        { PlayerStartsAtPos = Vector2(200f, 100f)
          PlayerStartsCarrying = []
          Tiles = tiles
          Size = (width, height) }


let levelLookup (level: Level) : LevelBuilder =
    match level with
    | L1 -> level1
    | L2 -> level2
    | L3 -> level3
