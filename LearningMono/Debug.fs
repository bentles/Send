module Debug

open Xelmish.Model
open Xelmish.Viewables

let debugText s (x, y) =
    let width, height = 120, 36
    text "defaultFont" 20. Colour.White (-0.5, -0.5) s (x + width / 2, y + height / 2)
