module EntityConfig

open Microsoft.Xna.Framework
open GameConfig


let timerImage =
    { SpriteSize = (50, 50)
      Rows = 1
      Columns = 4
      TextureName = "timer"
      Offset = Vector2(0f, 10f) }

let idImage =
    { SpriteSize = (50, 50)
      Rows = 4
      Columns = 1
      TextureName = "id"
      Offset = Vector2(0f, 10f) }

let mergeImage = { idImage with TextureName = "merge" }
let compareImage = { idImage with TextureName = "compare" }
let mapImage = { idImage with TextureName = "map" }
let filterImage = { idImage with TextureName = "filter" }
let toggleOnImage = { idImage with TextureName = "toggleOn" }
let toggleOffImage = { idImage with TextureName = "toggleOff" }
let nextLevelImage = { idImage with TextureName = "nextLevel" }

let unitImage =
    { SpriteSize = (50, 50)
      Rows = 1
      Columns = 1
      TextureName = "unit"
      Offset = Vector2(0f, 0f) }


let buttonImage = { unitImage with TextureName = "button" }

let rockImage = { unitImage with TextureName = "rock" }

let boxOpenImage =
    { unitImage with
        TextureName = "boxOpen"
        Rows = 3 }

let boxClosedImage =
    { unitImage with
        TextureName = "boxClosed"
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
          Tint = Color.White
          FrameLength = 300L }

let idConfig =
    { Image = idImage
      Tint = Color.White
      FrameLength = 300L }

let idSpriteConfig = SingleSpriteConfig idConfig
let mergeSpriteConfig = SingleSpriteConfig { idConfig with Image = mergeImage }
let compareSpriteConfig = SingleSpriteConfig { idConfig with Image = compareImage }
let mapSpriteConfig = SingleSpriteConfig { idConfig with Image = mapImage }
let filterSpriteConfig = SingleSpriteConfig { idConfig with Image = filterImage }
let unitSpriteConfig = SingleSpriteConfig { idConfig with Image = unitImage }

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

let boxOpenSpriteConfig =
    SingleSpriteConfig
        { Image = boxOpenImage
          Tint = Color.White
          FrameLength = 300L }

let boxClosedSpriteConfig =
    SingleSpriteConfig
        { Image = boxClosedImage
          Tint = Color.White
          FrameLength = 300L }

let nextLevelSpriteConfig =
    SingleSpriteConfig
        { Image = nextLevelImage
          Tint = Color.White
          FrameLength = 300L }
