module Config

open Microsoft.Xna.Framework
open Xelmish.Model

//game config
let window = Windowed(1600, 900)
let AcceptableError = 0.001f

[<Measure>]
type pixels

[<Measure>]
type blocks

type WorldConfig =
    { TileWidth: int; WorldTileLength: int }

let worldConfig = { TileWidth = 50; WorldTileLength = 10 }

// player config
type AABBConfig = { Half: Vector2; Offset: Vector2 }

type PlayerConfig =
    { BigMaxVelocity: float32
      SmallMaxVelocity: float32
      Acc: float32
      Slow: float32
      AABBConfig: AABBConfig }

type ImageConfig =
    { PixelSize: int * int
      Rows: int
      Columns: int
      Offset: Vector2
      TextureName: string }

type AnimationConfig =
    { Index: int
      Looping: bool
      Speed: int
      Columns: int }

type AnimatedSpriteConfig =
    { Images: ImageConfig list
      InitAnimation: AnimationConfig
      Tint: Color
      FrameLength: int64 }

type SingleSpriteConfig =
    { Image: ImageConfig
      Tint: Color
      FrameLength: int64 }

type SpriteConfig =
    | SingleSpriteConfig of SingleSpriteConfig
    | AnimatedSpriteConfig of AnimatedSpriteConfig

let playerConfig =
    { BigMaxVelocity = 2f
      SmallMaxVelocity = 4.5f
      Acc = 30f
      Slow = 40f
      AABBConfig =
        { Half = Vector2(25f, 25f)
          Offset = Vector2(0f, 20f) } }

let bigCharImage =
    { PixelSize = (800, 312)
      Rows = 3
      Columns = 8
      TextureName = "bigChar"
      Offset = Vector2.Zero }

let smallCharImage =
    { PixelSize = (416, 192)
      Rows = 3
      Columns = 8
      TextureName = "smallChar"
      Offset = Vector2(0f, -18f) }

let timerImage =
    { PixelSize = (200, 50)
      Rows = 1
      Columns = 4
      TextureName = "timer"
      Offset = Vector2(0f, 0f) }

let observerImage =
    { PixelSize = (50, 50)
      Rows = 1
      Columns = 1
      TextureName = "observer"
      Offset = Vector2(0f, 0f) }

let rockImage =
    { PixelSize = (50, 50)
      Rows = 1
      Columns = 1
      TextureName = "rock"
      Offset = Vector2(0f, 0f) }

let imageSpriteConfig =
    { Looping = false
      Speed = 0
      Index = 0
      Columns = 1 }

let CharAnimations =
    {| SmallWalk =
        { Index = 0
          Looping = true
          Speed = 120
          Columns = 8 }
       SmallToBig =
        { Index = 3
          Looping = false
          Speed = 80
          Columns = 8 }
       BigToSmall =
        { Index = 4
          Looping = false
          Speed = 80
          Columns = 8 }
       BigWalk =
        { Index = 5
          Looping = true
          Speed = 80
          Columns = 8 } |}

let CharConfig = {| BigSpeed = 80; SmallSpeed = 120 |}

let charSprite: SpriteConfig =
    AnimatedSpriteConfig
        { Images = [ smallCharImage; bigCharImage ]
          InitAnimation = CharAnimations.SmallWalk
          Tint = Color.White
          FrameLength = 300L }

//entities
let timerSpriteConfig =
    AnimatedSpriteConfig
        { Images = [ timerImage ]
          InitAnimation =
            { Index = 0
              Looping = true
              Speed = 80
              Columns = 4 }
          Tint = Color.White
          FrameLength = 300L }

let observerSpriteConfig =
    SingleSpriteConfig
        { Image = observerImage
          Tint = Color.White
          FrameLength = 300L }

let rockSpriteConfig =
    SingleSpriteConfig
        { Image = rockImage
          Tint = Color.White
          FrameLength = 300L }
