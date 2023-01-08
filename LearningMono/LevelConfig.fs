module LevelConfig

open Microsoft.Xna.Framework
open Collision
open System
open Entity

// Level primitives
type FloorType =
    | Empty
    | Grass

[<Struct>]
type Tile =
    { FloorType: FloorType
      Collider: AABB option

      Subscriptions: Tile list
      Action: (EntityType -> EntityType) option
      Entity: Entity.Model option       
    }

let defaultTile = {
    FloorType = FloorType.Empty
    Collider = None
    Subscriptions = []
    Action = None
    Entity = None
}

type LevelConfig = 
    {
        PlayerStartPos: Vector2
        LevelBuilder: unit -> Tile[]
    }

//let level1Config = {
//    PlayerStartPos = Vector2
//}
