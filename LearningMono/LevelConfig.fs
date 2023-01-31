module LevelConfig

open Microsoft.Xna.Framework
open Collision
open Entity
open Utility
open Config

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

type LevelConfig =
    { PlayerStartPos: Vector2
      LevelBuilder: unit -> Tile[] }

let createCollidableTile t xx yy =
    { defaultTile with
        FloorType = t
        Collider = Some(createColliderFromCoords xx yy half) }

let createNonCollidableTile t = { defaultTile with FloorType = t }

let createTimerOnGrass (coords: Vector2) time =
    let pos = coordsToPos coords.X coords.Y half

    let subject =
        Entity.Subject
            { Type = Entity.Timer ([Rock; buildObserver Id; buildObserver (Map Rock)], 60)
              TicksSinceEmit = 0
              GenerationNumber = 0
              ToEmit = Nothing }

    { defaultTile with
        FloorType = FloorType.Grass
        Entity = Some(Entity.init subject pos time Entity.FacingRight) }

let createObserverOnGrass (coords: Vector2) time observer : Tile =
    let pos = coordsToPos coords.X coords.Y half

    { defaultTile with
        FloorType = FloorType.Grass
        Entity = Some(Entity.init observer pos time Entity.FacingRight) }
