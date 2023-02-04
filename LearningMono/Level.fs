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
      Size: (int * int)
      }

type LevelBuilder = int64 -> Level

// helpers
let createCollidableTile t xx yy =
    { defaultTile with
        FloorType = t
        Collider = Some(createColliderFromCoords xx yy half) }

let createNonCollidableTile t = { defaultTile with FloorType = t }

let createTimerOnGrass (coords: Vector2) time =
    let pos = coordsToVector coords.X coords.Y half

    let subject =
        Entity.Subject
            { Type = Entity.Timer ([Rock; buildObserver Id; buildObserver (Map Rock); rockTimer], 60)
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

let level1: LevelBuilder = 
    fun time ->
        let width = 20
        let height = 10

        let tiles =
            seq {
                for yy in 0 .. (width - 1) do
                    for xx in 0 .. (height - 1) do
                        let grassTile = createNonCollidableTile FloorType.Grass
                    
                        match xx, yy with
                        | 0, 0 -> createNonCollidableTile FloorType.Grass
                        | 2, 2 -> createTimerOnGrass (Vector2(2f)) time
                        | _ -> grassTile
            }
            |> PersistentVector.ofSeq
        {
            PlayerStartsAtPos = Vector2(2f, 2f)
            PlayerStartsCarrying = []
            Tiles = tiles
            Size = (width, height)
        }

let level2: LevelBuilder = 
    fun time ->
        let width = 20
        let height = 10

        let tiles =
            seq {
                for yy in 0 .. (width - 1) do
                    for xx in 0 .. (height - 1) do
                        let grassTile = createNonCollidableTile FloorType.Grass
                    
                        match xx, yy with
                        | 0, 0 -> createNonCollidableTile FloorType.Grass
                        | 2, 2 -> createTimerOnGrass (Vector2(2f)) time
                        | _ -> grassTile
            }
            |> PersistentVector.ofSeq
        {
            PlayerStartsAtPos = Vector2(2f, 2f)
            PlayerStartsCarrying = []
            Tiles = tiles
            Size = (width, height)
        }
