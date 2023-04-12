[<RequireQualifiedAccess>]
module Keyboard
open Xelmish.Model
open Microsoft.Xna.Framework
   
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