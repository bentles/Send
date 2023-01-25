module Entity

open Collision
open Config
open Microsoft.Xna.Framework

type Facing = FacingUp | FacingDown | FacingLeft | FacingRight

type ObservableType =
    | Id
    | Map
    | Filter

type SubjectType = | Timer

type EntityType =
    | Rock
    | Subject of SubjectData
    | Observable of ObservableData

and ObservableData =
    { Type: ObservableType
      ToEmit: Emit
      TicksSinceEmit: int
      Observing: int option
      Action: (ObservableData -> EntityType -> ObservableData) }

and SubjectData =
    { Type: SubjectType
      ToEmit: Emit
      TicksSinceEmit: int
      GenerationNumber: int
      Action: (SubjectData -> SubjectData) }

and Emit =
    | WillEmit of EntityType
    | Emitting of EntityType
    | Emitted of EntityType
    | Nothing

type Model =
    { Sprite: Sprite.Model
      Collider: AABB option
      Facing: Facing
      Type: EntityType }

let getSpriteConfig (eType: EntityType) : SpriteConfig =
    match eType with
    | Rock -> rockSpriteConfig
    | Subject _ -> timerSpriteConfig
    | Observable { Type = ob } ->
        match ob with
        | Id -> idSpriteConfig
        | Map -> mapSpriteConfig
        | Filter -> filterSpriteConfig

let getEmitImage (eType: EntityType) =
    match eType with
    | Rock -> rockImage
    | Subject _ -> timerImage
    | Observable { Type = ob } ->
        match ob with
        | Id -> idImage
        | Map -> mapImage
        | Filter -> filterImage

let getCollider (eType: EntityType) (pos: Vector2) : AABB =
    match eType with
    | Rock -> { Pos = pos; Half = Vector2(10f, 10f) }
    | Subject _ -> { Pos = pos; Half = Vector2(10f, 10f) }
    | Observable _ -> { Pos = pos; Half = Vector2(10f, 10f) }


let (|Emittinged|_|) (emit:Emit) =
     match emit with
     | Emitting e
     | Emitted e -> Some (e)
     | _ -> None

let (|EmittingedObservable|_|) (emit:EntityType) =
     match emit with
     | Subject { ToEmit = Emittinged e; TicksSinceEmit = t } 
     | Observable { ToEmit = Emittinged e; TicksSinceEmit = t } -> Some (e, t)
     | _ -> None

let (|EmittingObservable|_|) (emit:EntityType) =
     match emit with
     | Subject { ToEmit = Emitting e; TicksSinceEmit = t } 
     | Observable { ToEmit = Emitting e; TicksSinceEmit = t } -> Some (e, t)
     | _ -> None

let init (entityType: EntityType) (pos: Vector2) (time) (facing:Facing) =
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
      Collider = Some collider }

let initNoCollider (entityType: EntityType) (pos: Vector2) time (facing:Facing) =
    { Type = entityType
      Sprite = Sprite.init pos time (getSpriteConfig entityType) None None
      Facing = facing
      Collider = None }

let withTarget (entityType:EntityType) (target: int option) =
    match entityType with
    | Observable obs -> Observable { obs with Observing = target }
    | other -> other