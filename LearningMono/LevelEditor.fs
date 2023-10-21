[<RequireQualifiedAccess>]
module LevelEditor

open Level

type Model = { World: voption<World> }

let init () = { World = ValueNone }
