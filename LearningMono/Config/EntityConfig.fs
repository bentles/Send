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
      Rows = 3
      Columns = 1
      TextureName = "id"
      Offset = Vector2(0f, 0f) }

let mapImage = { idImage with TextureName = "map" }
let filterImage = { idImage with TextureName = "filter" }
let toggleOnImage = { idImage with TextureName = "toggleOn" }
let toggleOffImage = { idImage with TextureName = "toggleOff" }

let emptyImage =
    { SpriteSize = (50, 50)
      Rows = 1
      Columns = 1
      TextureName = "empty"
      Offset = Vector2(0f, 0f) }

let nextLevelImage = { emptyImage with TextureName = "nextLevel" }

let buttonImage = { emptyImage with TextureName = "button" }

let rockImage = { emptyImage with TextureName = "rock" }

let boxImage =
    { emptyImage with
        TextureName = "box"
        Rows = 2 }

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
        { Image = emptyImage
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
