module LevelConfig

open Microsoft.Xna.Framework
open Collision
open Entity

// Level primitives
type FloorType =
    | Empty
    | Grass

type Observable =
    { Subscriptions: int list

      //unit -> EntityType and then use a closure??
      Action: (EntityType -> EntityType) }

type Subject =
    { Subscriptions: int list
      ToEmit: EntityType option
      TicksSinceLastGeneration: int
      GenerationNumber: int }

let buildRepeatListEmitEvery (list: EntityType list) (every: int) =
    let length = list.Length

    fun (subject: Subject) ->
        if subject.TicksSinceLastGeneration > every then
            let index = subject.GenerationNumber % length
            let itemToEmit = List.item index list

            { subject with
                TicksSinceLastGeneration = 0
                GenerationNumber = subject.GenerationNumber + 1
                ToEmit = Some itemToEmit }
        else
            { subject with
                TicksSinceLastGeneration = subject.TicksSinceLastGeneration + 1
                ToEmit = None }

let buildRepeatItemEmitEvery (item: EntityType) (every: int) =
    buildRepeatListEmitEvery [item] every


type Reactive =
    | Observable of Observable
    | Subject of Subject * (Subject -> Subject)

[<Struct>]
type Tile =
    { FloorType: FloorType
      Collider: AABB option
      Entity: Entity.Model option

      Reactive: Reactive option }

let defaultTile =
    { FloorType = FloorType.Empty
      Collider = None
      Reactive = None
      Entity = None }

type LevelConfig =
    { PlayerStartPos: Vector2
      LevelBuilder: unit -> Tile[] }

//let level1Config = {
//    PlayerStartPos = Vector2
//}
