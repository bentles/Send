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

[<Struct>]
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

[<Struct>]
type PlayerWorldInteraction =
    | TryPickup
    | TryPlace
    | NoAction

module Direct =
    /// Run the given event if the given key has just been pressed
    let onkeydown key event (inputs:Inputs) =
        if inputs.keyboardState.IsKeyDown key 
        && not (inputs.lastKeyboardState.IsKeyDown key) then 
            event ()

    /// Run the given event if the given key is currently pressed
    let whilekeydown key event (inputs:Inputs) =
        if inputs.keyboardState.IsKeyDown key 
        && inputs.lastKeyboardState.IsKeyDown key then 
            event ()

    /// Run the given event if the given key has just been released
    let onkeyup key event (inputs:Inputs)  =
        if not (inputs.keyboardState.IsKeyDown key)
        && inputs.lastKeyboardState.IsKeyDown key then 
            event ()
