module Prelude

open Microsoft.Xna.Framework
open Xelmish.Model

[<Struct>]
type Facing =
    | FacingUp
    | FacingDown
    | FacingLeft
    | FacingRight

[<Struct>]
type AABB = { Pos: Vector2; Half: Vector2 }

type Coords = (struct(int * int))
type CoordsF = (struct(float32 * float32))

[<Struct>]
type PhysicsInfo =
    { Time: int64
      PossibleObstacles: AABB seq
      Dt: float32 }

[<Struct>]
type PlayerWorldInteraction =
    | TryPickup
    | TryPlace
    | TryMultiPlace of start:bool
    | TryOrient of Facing
    | NoAction
