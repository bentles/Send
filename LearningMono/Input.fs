module Input

open Xelmish.Viewables
open Microsoft.Xna.Framework

let directions up down left right event =
    onupdate (fun inputs ->
        let y = if inputs.keyboardState.IsKeyDown up then -1 else 0
        let y = y + if inputs.keyboardState.IsKeyDown down then 1 else 0
        let x = if inputs.keyboardState.IsKeyDown left then -1 else 0
        let x = x + if inputs.keyboardState.IsKeyDown right then 1 else 0
        event (Vector2(float32 x, float32 y)))
