module Entity

open Collision
open Config
open Microsoft.Xna.Framework

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

let init (entityType: EntityType) (pos: Vector2) (time) =
    let sprite = Sprite.init pos time (getSpriteConfig entityType)
    let collider = getCollider entityType pos

    { Type = entityType
      Sprite = sprite
      Collider = Some collider }

let initNoCollider (entityType: EntityType) (pos: Vector2) time =
    { Type = entityType
      Sprite = Sprite.init pos time (getSpriteConfig entityType)
      Collider = None }

let withTarget (entityType:EntityType) (target: int option) =
    match entityType with
    | Observable obs -> Observable { obs with Observing = target }
    | other -> other