module Config

open Microsoft.Xna.Framework

type SpriteConfig =
    { PixelSize: int * int
      Rows: int
      Columns: int
      InitPos: int * int
      TextureName: string
      Tint: Color
      FrameLength: int64 }

let bigCharSprite =
    { PixelSize = (800, 312)
      Rows = 3
      Columns = 8
      InitPos = (0, 1)
      TextureName = "bigChar"
      Tint = Color.White
      FrameLength = 300L }

let smallCharSprite =
    { PixelSize = (416, 192)
      Rows = 3
      Columns = 8
      InitPos = (0, 0)
      TextureName = "smallChar"
      Tint = Color.White
      FrameLength = 300L }