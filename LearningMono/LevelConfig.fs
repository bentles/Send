module LevelConfig

open Microsoft.Xna.Framework
open Collision
open Entity
open Utility
open Config

// Level primitives
type FloorType =
    | Empty
    | Grass

and Tile =
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

let buildRepeatListEmittingEvery (list: EntityType list) (every: int) =
    let length = list.Length

    fun (subject: Subject) ->
        if subject.TicksSinceEmit > every then
            let index = subject.GenerationNumber % length
            let itemToEmit = List.item index list

            { subject with
                TicksSinceEmit = 0
                GenerationNumber = subject.GenerationNumber + 1
                ToEmit = WillEmit itemToEmit }
        else
            { subject with
                TicksSinceEmit = subject.TicksSinceEmit + 1
                ToEmit =
                    match subject.ToEmit with
                    | WillEmit t ->
                        printfn "%A" subject.ToEmit
                        Emitting t
                    | Emitting t -> Emitted t
                    | other -> other }

let buildRepeatItemEmitEvery (every: int) (item: EntityType) =
    buildRepeatListEmittingEvery [ item ] every

let rockTimer: EntityType =
    SubjectType(
        Timer,
        { ToEmit = Nothing
          TicksSinceEmit = 0
          GenerationNumber = 0
          Action = buildRepeatItemEmitEvery 30 Rock }
    )

let buildObserverObserving (observerFunc: EntityType -> Emit) (oType: ObservableType) (target: int option) : EntityType =
    let observerFunc (observable: Observable) (observing: Entity.EntityType) =
        //if you have your own stuffs do that
        let toEmit =
            match observable.ToEmit with
            | WillEmit t -> Emitting t
            | Emitting t -> Emitted t
            | other -> other

        //otherwise check in on the thing you are observing
        let toEmit =
            match toEmit with
            | Nothing
            | Emitted _ as cur ->
                match observing with
                | SubjectType(_, { ToEmit = (Emitting s) })
                | ObservableType(_, { ToEmit = (Emitting s) }) -> observerFunc s
                | _ -> cur
            | _ -> toEmit

        let ticks =
            match toEmit with
            | Emitting t -> 0
            | _ -> observable.TicksSinceEmit + 1

        { observable with
            ToEmit = toEmit
            TicksSinceEmit = ticks }

    ObservableType(
        oType,
        { ToEmit = Nothing
          Action = observerFunc
          TicksSinceEmit = 0
          Observing = target }
    )

let buildObserver (observerFunc: EntityType -> Emit) (oType: ObservableType) =
    buildObserverObserving observerFunc oType None

let idObservable = buildObserverObserving (fun e -> Emitting e) Id

let mapToTimer = buildObserverObserving (fun e -> Emitting rockTimer) Map

let onlyRock =
    buildObserverObserving (fun e ->
        match e with
        | Rock ->
            printfn "observing %A" e
            WillEmit e
        | _ -> Nothing) Filter

let tileHalf = float32 (worldConfig.TileWidth / 2)
let half = Vector2(tileHalf)

let createCollidableTile t xx yy =
    { defaultTile with
        FloorType = t
        Collider = Some(createColliderFromCoords xx yy half) }

let createNonCollidableTile t = { defaultTile with FloorType = t }

let createTimerOnGrass (coords: Vector2) time =
    let pos = coordsToPos coords.X coords.Y half
    let listEmitter = buildRepeatListEmittingEvery [ Rock; rockTimer; rockTimer ] 60

    let subject =
        Entity.SubjectType(
            Entity.Timer,
            { TicksSinceEmit = 0
              GenerationNumber = 0
              ToEmit = Nothing
              Action = listEmitter }
        )

    { defaultTile with
        FloorType = FloorType.Grass
        Entity = Some(Entity.init subject pos time) }

let createObserverOnGrass (coords: Vector2) time observer:Tile =
    let pos = coordsToPos coords.X coords.Y half

    { defaultTile with
        FloorType = FloorType.Grass
        Entity = Some(Entity.init observer pos time) }
