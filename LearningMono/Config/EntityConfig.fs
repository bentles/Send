module EntityConfig

open Microsoft.Xna.Framework
open GameConfig


let timerImage =
    { SpriteSize = (50, 50)
      Rows = 1
      Columns = 4
      TextureName = "timer"
      Offset = Vector2(0f, 0f) }

let idImage =
    { SpriteSize = (50, 50)
      Rows = 4
      Columns = 1
      TextureName = "id"
      Offset = Vector2(0f, 0f) }

let mergeImage = { idImage with TextureName = "merge" }
let mapImage = { idImage with TextureName = "map" }
let filterImage = { idImage with TextureName = "filter" }
let toggleOnImage = { idImage with TextureName = "toggleOn" }
let toggleOffImage = { idImage with TextureName = "toggleOff" }

let unitImage =
    { SpriteSize = (50, 50)
      Rows = 1
      Columns = 1
      TextureName = "unit"
      Offset = Vector2(0f, 0f) }

let nextLevelImage = { unitImage with TextureName = "nextLevel" }

let buttonImage = { unitImage with TextureName = "button" }

let rockImage = { unitImage with TextureName = "rock" }

let boxImage =
    { unitImage with
        TextureName = "box"
        Rows = 3 }

//entities
let timerSpriteConfig =
    AnimatedSpriteConfig
        { Images = [ timerImage ]
          InitAnimation =
            { Index = 0
              Looping = true
              Speed = 300
              Columns = 4 }
          Started = true
          Tint = Color.White
          FrameLength = 300L }

let buttonSpriteConfig =
    SingleSpriteConfig
        { Image = buttonImage
          Tint = Color.LawnGreen
          FrameLength = 300L }

let idConfig =
    { Image = idImage
      Tint = Color.White
      FrameLength = 300L }

let idSpriteConfig = SingleSpriteConfig idConfig
let mergeSpriteConfig = SingleSpriteConfig { idConfig with Image = mergeImage }
let mapSpriteConfig = SingleSpriteConfig { idConfig with Image = mapImage }
let filterSpriteConfig = SingleSpriteConfig { idConfig with Image = filterImage }

let toggleOffSpriteConfig =
    SingleSpriteConfig { idConfig with Image = toggleOffImage }

let toggleOnSpriteConfig =
    SingleSpriteConfig { idConfig with Image = toggleOnImage }

let rockSpriteConfig =
    SingleSpriteConfig
        { Image = rockImage
          Tint = Color.White
          FrameLength = 300L }

let emptySpriteConfig =
    SingleSpriteConfig
        { Image = unitImage
          Tint = Color.White
          FrameLength = 300L }

let boxSpriteConfig =
    SingleSpriteConfig
        { Image = boxImage
          Tint = Color.White
          FrameLength = 300L }

let nextLevelSpriteConfig =
    SingleSpriteConfig
        { Image = nextLevelImage
          Tint = Color.LawnGreen
          FrameLength = 300L }
