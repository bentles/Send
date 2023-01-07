module Entity

open Collision
open Config
open Microsoft.Xna.Framework

type EntityType =
    | Rock
    | Timer
    | Observer

type Model =
    { Sprite: Sprite.Model
      Collider: AABB option
      Type: EntityType }

let getSpriteConfig (eType: EntityType) : SpriteConfig =
    match eType with
    | Rock -> rockSpriteConfig
    | Timer -> timerSpriteConfig
    | Observer -> observerSpriteConfig

let getCollider (eType: EntityType) (pos: Vector2) : AABB =
    match eType with
    | Rock -> { Pos = pos; Half = Vector2(10f, 10f) }
    | Timer -> { Pos = pos; Half = Vector2(10f, 10f) }
    | Observer -> { Pos = pos; Half = Vector2(10f, 10f) }

let init (entityType: EntityType) (pos: Vector2) (time) =
    let sprite = Sprite.init pos time (getSpriteConfig entityType)
    let collider = getCollider entityType pos
    { 
      Type = entityType
      Sprite = sprite
      Collider = Some collider }

let initNoCollider (entityType: EntityType) (pos: Vector2) time =
    { Type = entityType
      Sprite = Sprite.init pos time (getSpriteConfig entityType)
      Collider = None }
