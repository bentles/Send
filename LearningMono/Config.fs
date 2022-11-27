module Config

open Microsoft.Xna.Framework

type ImageConfig =
    { PixelSize: int * int
      Rows: int
      Columns: int
      TextureName: string }

type SpriteConfig =
    { Images: ImageConfig list

      InitPos: int * int
      Tint: Color
      FrameLength: int64 }

let bigCharImage =
    { PixelSize = (800, 312)
      Rows = 3
      Columns = 8
      TextureName = "bigChar" }

let smallCharImage =
    { PixelSize = (416, 192)
      Rows = 3
      Columns = 8
      TextureName = "smallChar" }

let CharAnimations = {|
    BigWalk = 4
    SmallWalk = 0
|}

let charSprite =
    {
      Images = [
        smallCharImage
        bigCharImage
      ]

      InitPos = (0, CharAnimations.SmallWalk)
      Tint = Color.White
      FrameLength = 300L }

