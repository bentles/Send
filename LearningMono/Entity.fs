﻿module Entity

open GameConfig
open Microsoft.Xna.Framework
open Prelude
open EntityConfig

[<Struct>]
type Emit<'a> =
    | WillEmit of will: 'a
    | Emitting of emitting: 'a
    | Emitted of emitted: 'a
    | Nothing

[<Struct>]
type InteractionEvent =
    | GoToLevel of Level
    | NoEvent
//  | Menu for to configure the things
//  | Change the state of the thing

[<Struct>]
type SubjectType = Timer of BoxType * int

and BoxType = EntityType list

and ObservableType =
    | Id
    | Map of EntityType
    | Filter of EntityType
    | Compare

and EntityType =
    | Rock
    | GoToLevelButton of Level
    | Box of BoxType
    | Subject of SubjectData
    | Observable of ObservableData

and ObservableData =
    { Type: ObservableType
      ToEmit: Emit<EntityType>
      TicksSinceEmit: int
      Observing: int option
      Observing2: int option }

and SubjectData =
    { Type: SubjectType
      ToEmit: Emit<EntityType>
      TicksSinceEmit: int
      GenerationNumber: int }

[<Struct>]
type Model =
    { Sprite: Sprite.Model
      Collider: AABB option
      CanBePickedUp: bool
      Facing: Facing
      Type: EntityType }

let getSpriteConfig (eType: EntityType) : SpriteConfig =
    match eType with
    | Rock -> rockSpriteConfig
    | GoToLevelButton _ -> nextLevelSpriteConfig
    | Box _ -> rockSpriteConfig
    | Subject _ -> timerSpriteConfig
    | Observable { Type = ob } ->
        match ob with
        | Id -> idSpriteConfig
        | Map _ -> mapSpriteConfig
        | Filter _ -> filterSpriteConfig
        | Compare -> filterSpriteConfig

let getEmitImage (eType: EntityType) =
    match eType with
    | Rock -> rockImage
    | GoToLevelButton _ -> nextLevelImage
    | Box _ -> rockImage
    | Subject _ -> timerImage
    | Observable { Type = ob } ->
        match ob with
        | Id -> idImage
        | Map _ -> mapImage
        | Filter _ -> filterImage
        | Compare -> filterImage

let rec entityEq (e1: EntityType) (e2: EntityType) =
    match (e1, e2) with
    | Rock, Rock -> true
    | Box _, Box _ -> true // check contents ??
    | Observable { Type = Id }, Observable { Type = Id } -> true
    | Observable { Type = Map _ }, Observable { Type = Map _ } -> true
    | Observable { Type = Filter _ }, Observable { Type = Filter _ } -> true
    | Subject { Type = Timer _ }, Subject { Type = Timer _ } -> true
    | _, _ -> false

let getCollider (eType: EntityType) (pos: Vector2) : AABB option =
    match eType with
    | GoToLevelButton _ -> None
    | Rock
    | Subject _
    | Box _
    | Observable _ -> Some { Pos = pos; Half = Vector2(10f, 10f) }

let (|Emittinged|_|) (emit: Emit<'a>) =
    match emit with
    | Emitting e
    | Emitted e -> Some(e)
    | _ -> None

let (|EmittingedObservable|_|) (emit: EntityType) =
    match emit with
    | Subject { ToEmit = Emittinged e
                TicksSinceEmit = t }
    | Observable { ToEmit = Emittinged e
                   TicksSinceEmit = t } -> Some(e, t)
    | _ -> None

let (|EmittingObservable|_|) (emit: EntityType) =
    match emit with
    | Subject { ToEmit = Emitting e
                TicksSinceEmit = t }
    | Observable { ToEmit = Emitting e
                   TicksSinceEmit = t } -> Some(e, t)
    | _ -> None


let getObserverFunc (obs: ObservableType) =
    let behaviorFunc (a: EntityType option) (b: EntityType option) =
        match obs with
        | Id ->
            match a with
            | (Some e1) -> WillEmit e1
            | _ -> Nothing
        | Map e ->
            match a with
            | (Some _) -> WillEmit e
            | _ -> Nothing
        | Filter e ->
            match a with
            | (Some e1) when (entityEq e e1) -> WillEmit e1
            | _ -> Nothing
        | Compare ->
            match (a, b) with
            | (Some e1), (Some e2) when (entityEq e1 e2) -> WillEmit e1
            | _ -> Nothing

    fun (observable: ObservableData) (observing: EntityType option) (observing2: EntityType option) ->
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
                let observed =
                    match observing with
                    | Some(EmittingObservable(s, _)) -> Some s
                    | _ -> None

                let observed2 =
                    match observing with
                    | Some(EmittingObservable(s, _)) -> Some s
                    | _ -> None

                let emit = behaviorFunc observed observed2

                match emit with
                | Nothing -> cur
                | _ -> emit
            | _ -> toEmit

        let ticks =
            match toEmit with
            | Emitting _ -> 0
            | _ -> observable.TicksSinceEmit + 1

        { observable with
            ToEmit = toEmit
            TicksSinceEmit = ticks }

let buildRepeatListEmittingEvery list (every: int) =
    let length = List.length list

    fun (subject: SubjectData) ->
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
                    | WillEmit t -> Emitting t
                    | Emitting t -> Emitted t
                    | other -> other }


let getSubjectFunc (sub: SubjectType) =
    match sub with
    | Timer(box, time) -> buildRepeatListEmittingEvery box time

let init (entityType: EntityType) (pos: Vector2) (time) (facing: Facing) (canBePickedUp: bool) =
    let config = (getSpriteConfig entityType)
    let rowsLastIndex = (getTotalRows config) - 1

    let ypos, flipped =
        match facing with
        | FacingLeft -> 0, true
        | FacingRight -> 0, false
        | FacingUp -> 2, false
        | FacingDown -> 1, false

    let ypos = min ypos rowsLastIndex

    let sprite = Sprite.init pos time config (Some ypos) (Some flipped)
    let collider = getCollider entityType pos

    { Type = entityType
      Sprite = sprite
      Facing = facing
      CanBePickedUp = canBePickedUp
      Collider = collider }

let initNoCollider (entityType: EntityType) (pos: Vector2) time (facing: Facing) (canBePickedUp: bool) =
    { Type = entityType
      Sprite = Sprite.init pos time (getSpriteConfig entityType) None None
      Facing = facing
      CanBePickedUp = canBePickedUp
      Collider = None }

let withTarget (entityType: EntityType) (target: int option) =
    match entityType with
    | Observable obs -> Observable { obs with Observing = target }
    | other -> other

let buildRepeatItemEmitEvery (every: int) (item: EntityType) =
    buildRepeatListEmittingEvery [ item ] every

let buildRockTimer =
    Subject
        { Type = Timer([ Rock ], 30)
          ToEmit = Nothing
          TicksSinceEmit = 0
          GenerationNumber = 0 }

let rockTimer: EntityType = buildRockTimer

let buildObserverObserving (oType: ObservableType) (target: int option) (target2: int option) : EntityType =
    Observable(
        { Type = oType
          ToEmit = Nothing
          TicksSinceEmit = 0
          Observing = target
          Observing2 = target2 }
    )

let buildObserver (oType: ObservableType) = buildObserverObserving oType None None

let tileHalf = float32 (worldConfig.TileWidth / 2)
let half = Vector2(tileHalf)


let interact (entity: Model): Model * InteractionEvent = 
    match entity.Type with
    | GoToLevelButton l -> entity, InteractionEvent.GoToLevel l
    | _ -> entity, InteractionEvent.NoEvent