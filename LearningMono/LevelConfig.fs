module LevelConfig

open Microsoft.Xna.Framework
open Collision

// Level primitives
type FloorType =
    | Empty
    | Grass

type Tile =
    { FloorType: FloorType
      Collider: AABB option
      Entity: Entity.Model option }

type LevelConfig = 
    {
        PlayerStartPos: Vector2
        LevelBuilder: unit -> Tile[]
    }

//let level1Config = {
//    PlayerStartPos = Vector2
//}
