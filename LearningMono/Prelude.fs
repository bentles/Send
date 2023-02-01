module Prelude

open Microsoft.Xna.Framework

[<Struct>]
type Facing =
    | FacingUp
    | FacingDown
    | FacingLeft
    | FacingRight


type AABB = { Pos: Vector2; Half: Vector2 }