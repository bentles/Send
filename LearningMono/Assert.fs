module Assert

open Microsoft.Xna.Framework
open Config

let inputAffectsVelocityAssertions (input: Vector2) (oldVel: Vector2) (newVel: Vector2) : bool =
    if input = Vector2.Zero then
        newVel.Length() <= oldVel.Length() + AcceptableError
    else
        Vector2.Dot(input, newVel) >= Vector2.Dot(input, newVel) - AcceptableError