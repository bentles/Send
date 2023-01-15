module Debug

open Xelmish.Model
open Xelmish.Viewables

let debugText s (x, y) =
    text "defaultFont" 20. Colour.White (0, 0) s (x , y)
