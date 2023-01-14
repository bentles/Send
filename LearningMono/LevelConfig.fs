module LevelConfig

open Microsoft.Xna.Framework
open Collision
open Entity

type Emit =
    | WillEmit of EntityType
    | Emitting of EntityType
    | Emitted of EntityType
    | Nothing

// Level primitives
type FloorType =
    | Empty
    | Grass

type Observable =
    { ToEmit: Emit
      TicksSinceEmit: int
      Observing: int }

type Subject =
    { ToEmit: Emit
      TicksSinceEmit: int
      GenerationNumber: int }

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

let buildRepeatItemEmitEvery (item: EntityType) (every: int) =
    buildRepeatListEmittingEvery [ item ] every

type Reactive =
    | Observable of Observable * (Observable -> Tile[] -> Observable)
    | Subject of Subject * (Subject -> Subject)

and Tile =
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


let buildObserver (observerFunc: EntityType -> Emit) =
    let observer (observable: Observable) (tiles: Tile[]) =
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
                let tile = Array.item observable.Observing tiles

                match tile.Reactive with
                | Some(Observable({ ToEmit = (Emitting s) }, _)) //ok lol this seems a bit much
                | Some(Subject({ ToEmit = (Emitting s) }, _)) -> observerFunc s
                | _ -> cur
            | _ -> toEmit

        let ticks =
            match toEmit with
            | Emitting t -> 0
            | _ -> observable.TicksSinceEmit + 1

        { observable with
            ToEmit = toEmit
            TicksSinceEmit = ticks }

    observer

let id = buildObserver (fun e -> Emitting e)

let mapToTimer = buildObserver (fun e -> Emitting Timer)

let onlyRock =
    buildObserver (fun e ->
        match e with
        | Rock ->
            printfn "observing %A" e
            WillEmit e
        | _ -> Nothing)
