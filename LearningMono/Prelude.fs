module Prelude

open Microsoft.Xna.Framework

[<Struct>]
type Facing =
    | FacingUp
    | FacingDown
    | FacingLeft
    | FacingRight


type AABB = { Pos: Vector2; Half: Vector2 }

type PhysicsInfo =
    { Time: int64
      PossibleObstacles: AABB seq
      Dt: float32 }

[<Struct>]
type Level =
    | L1
    | L2
    | L3
    | L4
