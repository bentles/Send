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

let rockImage =
    { SpriteSize = (50, 50)
      Rows = 1
      Columns = 1
      TextureName = "rock"
      Offset = Vector2(0f, 0f) }

let nextLevelImage = 
    { rockImage 
      with TextureName = "nextLevel"
    }

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

let idConfig =
    { Image = idImage
      Tint = Color.White
      FrameLength = 300L }

let idSpriteConfig = SingleSpriteConfig idConfig
let mapSpriteConfig = SingleSpriteConfig { idConfig with Image = mapImage }
let filterSpriteConfig = SingleSpriteConfig { idConfig with Image = filterImage }

let rockSpriteConfig =
    SingleSpriteConfig
        { Image = rockImage
          Tint = Color.White
          FrameLength = 300L }

let nextLevelSpriteConfig =
    SingleSpriteConfig
        { Image = nextLevelImage
          Tint = Color.LawnGreen
          FrameLength = 300L }