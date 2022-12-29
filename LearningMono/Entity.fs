module Entity

open Collision
open Config
open Microsoft.Xna.Framework

type Model =
    { Sprite: Sprite.Model
      Collider: AABB option }

type EntityType =
    | Rock
    | Timer
    | Observer

let init (spriteConfig: SpriteConfig) (pos: Vector2) (half: Vector2) (offset: Vector2) =
    let sprite = Sprite.init pos spriteConfig
    let collider = { Pos = pos; Half = half }

    { Sprite = sprite
      Collider = Some collider }

let initNoCollider (spriteConfig: SpriteConfig) (pos: Vector2) =
    { Sprite = Sprite.init pos spriteConfig 
      Collider = None  
    }