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
    | TryOrient of Facing
    | NoAction

[<RequireQualifiedAccess>]
module KeyBoard =
    let iskeydown key (inputs:Inputs) =
        inputs.keyboardState.IsKeyDown key 
        && not (inputs.lastKeyboardState.IsKeyDown key) 

    let whilekeydown key (inputs:Inputs) =
        inputs.keyboardState.IsKeyDown key 
        && inputs.lastKeyboardState.IsKeyDown key

    /// Run the given event if the given key has just been released
    let iskeyup key (inputs:Inputs)  =
        not (inputs.keyboardState.IsKeyDown key)
        && inputs.lastKeyboardState.IsKeyDown key

    let directions up down left right (inputs:Inputs) =
        let y = if inputs.keyboardState.IsKeyDown up then -1 else 0
        let y = y + if inputs.keyboardState.IsKeyDown down then 1 else 0
        let x = if inputs.keyboardState.IsKeyDown left then -1 else 0
        let x = x + if inputs.keyboardState.IsKeyDown right then 1 else 0
        Vector2(float32 x, float32 y)

