module LevelConfig

open Microsoft.Xna.Framework
open Collision
open Entity

type Emit =
    | WillEmit of EntityType
    | Emitting of EntityType

// Level primitives
type FloorType =
    | Empty
    | Grass

type Observable =
    { ToEmit: Emit option
      TicksSinceLastEmit: int
      Observing: int }

type Subject =
    { ToEmit: Emit option
      TicksSinceLastEmit: int
      GenerationNumber: int }

let buildRepeatListEmittingEvery (list: EntityType list) (every: int) =
    let length = list.Length

    fun (subject: Subject) ->
        if subject.TicksSinceLastEmit > every then
            let index = subject.GenerationNumber % length
            let itemToEmit = List.item index list

            { subject with
                TicksSinceLastEmit = 0
                GenerationNumber = subject.GenerationNumber + 1
                ToEmit = Some(WillEmit itemToEmit) }
        else
            { subject with
                TicksSinceLastEmit = subject.TicksSinceLastEmit + 1
                ToEmit =
                    match subject.ToEmit with
                    | Some(WillEmit t) -> 
                        printfn "%A" subject.ToEmit
                        Some(Emitting t)
                    | _ -> None }

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


let mapper (observable: Observable) (tiles:Tile[]) =
        //if you have your own stuffs do that
        let toEmit = match observable.ToEmit with
                            | Some(WillEmit t) ->                                
                                Some(Emitting t)
                            | _ -> None 
        
        //otherwise check in on the thing you are observing
        let toEmit =
            if toEmit.IsNone then
                let tile = Array.item observable.Observing tiles
 
                match tile.Reactive with
                | Some (Observable ({ ToEmit = Some (Emitting s) }, _)) //ok lol this seems a bit much 
                | Some (Subject ({ ToEmit = Some (Emitting s) }, _)) -> 
                    printfn "observing %A" s
                    Some (WillEmit s) 
                | _ -> None
            else toEmit

        { observable with ToEmit = toEmit }


let filterToRock (observable: Observable) (tiles:Tile[]) =
        //if you have your own stuffs do that
        let toEmit = match observable.ToEmit with
                            | Some(WillEmit t) ->                                
                                Some(Emitting t)
                            | _ -> None 
        
        //otherwise check in on the thing you are observing
        let toEmit =
            if toEmit.IsNone then
                let tile = Array.item observable.Observing tiles
 
                match tile.Reactive with
                | Some (Observable ({ ToEmit = Some (Emitting s) }, _)) //ok lol this seems a bit much 
                | Some (Subject ({ ToEmit = Some (Emitting s) }, _)) -> 
                    match s with 
                    | Rock -> 
                        printfn "observing %A" s
                        Some (WillEmit s)
                    | _ -> None
                | _ -> None
            else toEmit

        { observable with ToEmit = toEmit }
                


        
