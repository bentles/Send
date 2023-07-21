module Entity

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
    | NextLevel
    | NoEvent
//  | Menu for to configure the things
//  | Change the state of the thing


[<Struct>]
type ObserverType =
    | SingleObserver
    | DoubleObserver

[<Struct>]
type SubjectType =
    | Timer of BoxData * int
    | Button
    | Box of BoxData

and BoxData =
    { Items: (EntityType list)
      IsOpen: bool }

and ObservableType =
    | Id
    | GoToNextLevel
    | Toggle of bool
    | Map of EntityType
    | Filter of EntityType
    | Compare
    | Merge

and EntityType =
    | Unit
    | Rock
    // | NextLevel

    | Subject of SubjectData
    | Observable of ObservableData

and ObservableData =
    { Type: ObservableType
      ToEmit: Emit<EntityType>
      TicksSinceEmit: int }

and SubjectData =
    { Type: SubjectType
      ToEmit: Emit<EntityType>
      TicksSinceEmit: int
      GenerationNumber: int }

[<Struct>]
type Model =
    { Sprite: Sprite.Model
      Collider: AABB voption
      CanBePickedUp: bool
      Facing: Facing
      Type: EntityType }

let defaultSubjectData: SubjectData =
    { Type = Button
      ToEmit = Nothing
      TicksSinceEmit = 0
      GenerationNumber = 0 }

let getSpriteConfig (eType: EntityType) : SpriteConfig =
    match eType with
    | Unit -> emptySpriteConfig
    | Rock -> rockSpriteConfig
    | Subject { Type = sub } ->
        match sub with
        | Timer _ -> timerSpriteConfig
        | Button -> buttonSpriteConfig
        | Box { IsOpen = true } -> boxOpenSpriteConfig
        | Box { IsOpen = false } -> boxClosedSpriteConfig
    | Observable { Type = ob } ->
        match ob with
        | GoToNextLevel _ -> nextLevelSpriteConfig
        | Id -> idSpriteConfig
        | Toggle true -> toggleOnSpriteConfig
        | Toggle false -> toggleOffSpriteConfig
        | Map _ -> mapSpriteConfig
        | Merge -> mergeSpriteConfig
        | Filter _ -> filterSpriteConfig
        | Compare -> compareSpriteConfig

let getEmitImage (eType: EntityType) =
    match eType with
    | Unit -> unitImage
    | Rock -> rockImage
    | Subject { Type = sub } ->
        match sub with
        | Timer _ -> timerImage
        | Button -> buttonImage
        | Box { IsOpen = true } -> boxOpenImage
        | Box { IsOpen = false } -> boxClosedImage
    | Observable { Type = ob } ->
        match ob with
        | GoToNextLevel _ -> nextLevelImage
        | Toggle _ -> toggleOnImage
        | Id -> idImage
        | Map _ -> mapImage
        | Merge -> mergeImage
        | Filter _ -> filterImage
        | Compare -> compareImage

let rec entityEq (e1: EntityType) (e2: EntityType) =
    match (e1, e2) with
    | Rock, Rock -> true
    | Unit, Unit -> true
    | Observable { Type = Id }, Observable { Type = Id } -> true
    | Observable { Type = Toggle _ }, Observable { Type = Toggle _ } -> true
    | Observable { Type = Map _ }, Observable { Type = Map _ } -> true
    | Observable { Type = Filter _ }, Observable { Type = Filter _ } -> true
    | Observable { Type = Compare }, Observable { Type = Compare } -> true
    | Observable { Type = Merge }, Observable { Type = Merge } -> true
    | Subject { Type = Timer _ }, Subject { Type = Timer _ } -> true
    | Subject { Type = Button _ }, Subject { Type = Button _ } -> true
    | Subject { Type = Box { IsOpen = a} }, Subject { Type = Box {IsOpen = b} } when a = b -> true // check contents ?? check open/closed
    | _, _ -> false

let getCollider (eType: EntityType) (pos: Vector2) : AABB voption =
    match eType with
    | Unit
    | Observable { Type = Toggle false } -> ValueNone
    | Rock
    | Subject _
    | Observable _ -> ValueSome { Pos = pos; Half = Vector2(10f, 10f) }

let getObserverType (obs: ObservableType) : ObserverType =
    match obs with
    | Merge
    | Compare -> DoubleObserver
    | _ -> SingleObserver

let (|RenderEmitting|_|) (emit: Emit<'a>) =
    match emit with
    | Emitting e
    | Emitted e -> Some(e)
    | _ -> None

let (|RenderEmittingObservable|_|) (emit: EntityType) =
    match emit with
    | Subject { ToEmit = RenderEmitting e
                TicksSinceEmit = t }
    | Observable { ToEmit = RenderEmitting e
                   TicksSinceEmit = t } -> Some(e, t)
    | _ -> None

let (|EmittingObservable|_|) (emit: EntityType) =
    match emit with
    | Subject { ToEmit = Emitting e
                TicksSinceEmit = t }
    | Observable { ToEmit = Emitting e
                   TicksSinceEmit = t } -> Some(e, t)
    | _ -> None

[<return: Struct>]
let (|CanPlaceIntoEntity|_|) (inputEntity: EntityType) (entityType: EntityType) : voption<EntityType> =
    match entityType with
    | Observable({ Type = Map Unit } as oData) -> ValueSome(Observable { oData with Type = Map inputEntity })
    | Observable({ Type = Filter Unit } as oData) -> ValueSome(Observable { oData with Type = Filter inputEntity })
    | Subject({ Type = Box { IsOpen = true; Items = items } } as sData) ->
        ValueSome(
            Subject
                { sData with
                    Type =
                        Box
                            { IsOpen = true
                              Items = inputEntity :: items } }
        )
    | _ -> ValueNone

[<return: Struct>]
let (|CanPickOutOfEntity|_|) (entityType: EntityType) : voption<EntityType * EntityType> =
    match entityType with
    | Observable { Type = Map Unit }
    | Observable { Type = Filter Unit } -> ValueNone
    | Observable({ Type = Map e } as obs) -> ValueSome(Observable { obs with Type = Map Unit }, e)
    | Observable({ Type = Filter e } as obs) -> ValueSome(Observable { obs with Type = Filter Unit }, e)
    | Subject({ Type = Box { IsOpen = true; Items = e :: rest } } as sub) ->
        ValueSome(Subject { sub with Type = Box { IsOpen = true; Items = rest } }, e)
    | _ -> ValueNone


let takeOutOf (existingEntity: EntityType) =
    match existingEntity with
    | Observable({ Type = Map _ } as oData) -> Observable { oData with Type = Map Unit }
    | Observable({ Type = Filter _ } as oData) -> Observable { oData with Type = Filter Unit }
    | Subject({ Type = Box { IsOpen = true; Items = _ :: rest } } as sub) ->
        Subject { sub with Type = Box { IsOpen = true; Items = rest } }
    | _ -> existingEntity

let private behaviorFunc (observable: ObservableData) (a: EntityType voption) (b: EntityType voption) =
    match observable.Type with
    | Id
    | GoToNextLevel
    | Toggle _ ->
        match a with
        | (ValueSome e1) -> WillEmit e1
        | _ -> Nothing
    | Map e ->
        match a with
        | (ValueSome _) -> WillEmit e
        | _ -> Nothing
    | Filter e ->
        match a with
        | (ValueSome e1) when (entityEq e e1) -> WillEmit e1
        | _ -> Nothing
    | Compare ->
        match (a, b) with
        | (ValueSome e1), (ValueSome e2) when (entityEq e1 e2) -> WillEmit e1
        | _ -> Nothing
    | Merge ->
        match (a, b) with
        | (ValueSome e1), (ValueSome e2) ->
            WillEmit(Subject { defaultSubjectData with Type = Box { Items = [ e1; e2 ]; IsOpen = false } })
        | (ValueSome e1), ValueNone -> WillEmit e1
        | ValueNone, ValueSome e2 -> WillEmit e2
        | _ -> Nothing

let observerFunc (observable: ObservableData) (observing: EntityType voption) (observing2: EntityType voption) =

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
                | ValueSome(EmittingObservable(s, _)) -> ValueSome s
                | _ -> ValueNone

            let observed2 =
                match observing2 with
                | ValueSome(EmittingObservable(s, _)) -> ValueSome s
                | _ -> ValueNone

            let emit = behaviorFunc observable observed observed2

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

let getOnEmit (obs: EntityType) (pos: Vector2) =
    match obs with
    | Observable({ Type = Toggle state } as obData) ->
        fun (entity: Model) ->
            let newSprite =
                (if not state then
                     toggleOnSpriteConfig
                 else
                     toggleOffSpriteConfig)

            let newObs = Observable { obData with Type = (Toggle(not state)) }

            { entity with
                Type = newObs
                Collider = if not state then getCollider newObs pos else ValueNone
                Sprite = Sprite.reInit entity.Sprite newSprite ValueNone }
    | Observable({ Type = GoToNextLevel }) ->
        fun (entity: Model) ->
            let newObs = Unit

            { entity with
                Type = newObs
                Sprite = Sprite.reInit entity.Sprite unitSpriteConfig ValueNone }
    | _ -> (id)


let subjectStep (subject: SubjectData) =
    { subject with
        TicksSinceEmit = subject.TicksSinceEmit + 1
        ToEmit =
            match subject.ToEmit with
            | WillEmit t -> Emitting t
            | Emitting t -> Emitted t
            | other -> other }

let buildRepeatListEmittingEvery (box: BoxData) (every: int) =
    let length = List.length box.Items

    fun (subject: SubjectData) ->
        if subject.TicksSinceEmit > every then
            let index = subject.GenerationNumber % length
            let itemToEmit = List.item index box.Items

            { subject with
                TicksSinceEmit = 0
                GenerationNumber = subject.GenerationNumber + 1
                ToEmit = WillEmit itemToEmit }
        else
            subjectStep subject


let getSubjectFunc (sub: SubjectType) =
    match sub with
    | Timer(box, time) -> buildRepeatListEmittingEvery box time
    | Button _ -> subjectStep
    | Box _ -> subjectStep

let getBoxYPos (boxType:BoxData) = 
    match boxType with
    | { IsOpen = false } -> 0
    | { Items = _ :: _ } -> 1
    | { Items = [] } -> 0

let getYpos (entityType: EntityType) (facing: Facing) =
    match entityType with
    | Observable _ ->
        match facing with
        | FacingLeft -> 0
        | FacingRight -> 1
        | FacingDown -> 2
        | FacingUp -> 3
    | Subject { Type = Box boxData } -> getBoxYPos boxData
    | _ -> 0

let init (entityType: EntityType) (pos: Vector2) (time) (facing: Facing) (canBePickedUp: bool) =
    let config = (getSpriteConfig entityType)

    let ypos = getYpos entityType facing

    let sprite =
        Sprite.startAnimation (Sprite.init pos time config (Some ypos) (Some false))

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
      Collider = ValueNone }

let updateSprite (entity: Model) =
    let yPos = getYpos entity.Type entity.Facing
    { entity with Sprite = Sprite.switchAnimation ({ imageSpriteConfig with Index = yPos }, 0, false) entity.Sprite }

let buildRepeatItemEmitEvery (every: int) (item: EntityType) =
    buildRepeatListEmittingEvery { Items = [ item ]; IsOpen = false } every

let buildRockTimer =
    Subject
        { Type = Timer({ Items = [ Rock ]; IsOpen = false }, 30)
          ToEmit = Nothing
          TicksSinceEmit = 0
          GenerationNumber = 0 }

let buildSubject (sType: SubjectType) =
    Subject
        { Type = sType
          ToEmit = Nothing
          TicksSinceEmit = 0
          GenerationNumber = 0 }

let rockTimer: EntityType = buildRockTimer

let observing (oType: ObservableType) : EntityType =
    Observable(
        { Type = oType
          ToEmit = Nothing
          TicksSinceEmit = 0 }
    )

let buildObserver (oType: ObservableType) = observing oType

let tileHalf = float32 (WorldConfig.TileWidth / 2)
let half = Vector2(tileHalf)

let interact (entity: Model) : Model * InteractionEvent =
    match entity.Type with
    | Subject({ Type = Button } as subData) ->
        { entity with
            Type =
                Subject
                    { subData with
                        ToEmit = (WillEmit << Subject) { defaultSubjectData with Type = Button }
                        TicksSinceEmit = 0
                        GenerationNumber = subData.GenerationNumber + 1 } },
        InteractionEvent.NoEvent
    | Subject({ Type = Box({ IsOpen = isOpen } as boxType) } as subj) ->
        let boxData = { boxType with IsOpen = not isOpen }
        let animIndex = getBoxYPos boxData
        let boxImageConfig = if boxData.IsOpen then boxOpenSpriteConfig else boxClosedSpriteConfig

        let newSprite =
            Sprite.reInit entity.Sprite boxImageConfig (ValueSome animIndex)

        { entity with
            Type =
                Subject
                    { subj with
                        Type = Box boxData
                        ToEmit = (WillEmit << Subject) { defaultSubjectData with Type = Box boxData }
                        TicksSinceEmit = 0
                        GenerationNumber = subj.GenerationNumber + 1 }
            Sprite = newSprite },
        InteractionEvent.NoEvent
    | _ -> entity, InteractionEvent.NoEvent
