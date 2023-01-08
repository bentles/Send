module LevelConfig

open Microsoft.Xna.Framework
open Collision
open Entity

// Level primitives
type FloorType =
    | Empty
    | Grass

type Subject = {
    Subscriptions: int list
    Generate: (unit -> EntityType)
}

type Observable = {
    Subscriptions: int list

    //unit -> EntityType and then use a closure??
    Action: (EntityType -> EntityType)
}

let repeatList<'T> (list: 'T list) =
    Seq.initInfinite (fun i -> 
        let imod = i % list.Length
        List.item imod list
    )

type Reactive = | Observable of Observable | Subject of Subject | Unreactive

let getSubs (reactive:Reactive) = 
    match reactive with 
    | Observable ob -> ob.Subscriptions
    | Subject sub -> sub.Subscriptions
    | Unreactive -> []

[<Struct>]
type Tile =
    { FloorType: FloorType
      Collider: AABB option
      Entity: Entity.Model option       

      Reactive: Reactive
    }

let defaultTile = {
    FloorType = FloorType.Empty
    Collider = None
    Reactive = Unreactive
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
