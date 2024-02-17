[<RequireQualifiedAccess>]
module Controls
open Xelmish.Model
open Microsoft.Xna.Framework
open System
open Microsoft.Xna.Framework.Input
   
let isKeyDown key (inputs:Inputs) =
    inputs.keyboardState.IsKeyDown key 
    && not (inputs.lastKeyboardState.IsKeyDown key) 

let whileKeyDown key (inputs:Inputs) =
    inputs.keyboardState.IsKeyDown key 
    && inputs.lastKeyboardState.IsKeyDown key

/// Run the given event if the given key has just been released
let isKeyUp key (inputs:Inputs)  =
    not (inputs.keyboardState.IsKeyDown key)
    && inputs.lastKeyboardState.IsKeyDown key

let directions up down left right (inputs:Inputs) =
    let y = if inputs.keyboardState.IsKeyDown up then -1 else 0
    let y = y + if inputs.keyboardState.IsKeyDown down then 1 else 0
    let x = if inputs.keyboardState.IsKeyDown left then -1 else 0
    let x = x + if inputs.keyboardState.IsKeyDown right then 1 else 0
    Vector2(float32 x, float32 y)

let isButtonDown (button:Buttons) (inputs:Inputs) =
    inputs.gamepadState.IsButtonDown button 
    && not (inputs.lastGamepadState.IsButtonDown button) 

let whileButtonDown (button:Buttons) (inputs:Inputs) =
    inputs.gamepadState.IsButtonDown button 
    && inputs.lastGamepadState.IsButtonDown button

/// Run the given event if the given button has just been released
let isButtonUp (button:Buttons) (inputs:Inputs)  =
    not (inputs.gamepadState.IsButtonDown button)
    && inputs.lastGamepadState.IsButtonDown button

let leftStick (inputs:Inputs) =
    inputs.gamepadState.ThumbSticks.Left * Vector2(1f, -1f)

let rightStick (inputs:Inputs) =
    inputs.gamepadState.ThumbSticks.Right * Vector2(1f, -1f)