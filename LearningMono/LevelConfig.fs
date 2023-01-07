module LevelConfig

open Microsoft.Xna.Framework
open Collision
open System
open Entity

// Level primitives
type FloorType =
    | Empty
    | Grass

type Producer = {
    Observable: IObservable<EntityType>

}

type Tile =
    { FloorType: FloorType
      Collider: AABB option
      Observable: IObservable<EntityType> option
      Entity: Entity.Model option }

type LevelConfig = 
    {
        PlayerStartPos: Vector2
        LevelBuilder: unit -> Tile[]
    }

//let level1Config = {
//    PlayerStartPos = Vector2
//}
