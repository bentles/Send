module PlayerConfig

open Prelude
open Microsoft.Xna.Framework
open GameConfig

// player config

type PlayerConfig =
    { BigMaxVelocity: float32
      SmallMaxVelocity: float32
      Acc: float32
      Slow: float32
      AABBConfig: AABB }


let playerConfig =
    { BigMaxVelocity = 2f
      SmallMaxVelocity = 4.5f
      Acc = 30f
      Slow = 40f
      AABBConfig =
        { Half = Vector2(25f, 25f)
          Pos = Vector2(0f, 20f) } }

let bigCharImage =
    { SpriteSize = (100, 104)
      Rows = 7
      Columns = 8
      TextureName = "bigChar"
      Offset = Vector2.Zero }

let smallCharImage =
    { SpriteSize = (52, 64)
      Rows = 4
      Columns = 8
      TextureName = "smallChar"
      Offset = Vector2(0f, -18f) }

let CharConfig = {| BigFrames = 50; SmallFrames = 90 |}

let CharAnimations =
    {| SmallWalk =
        { Index = 1
          Looping = true
          Speed = CharConfig.SmallFrames
          Columns = 8 }
       SmallToBig =
        { Index = 7
          Looping = false
          Speed = CharConfig.BigFrames
          Columns = 8 }
       BigToSmall =
        { Index = 5
          Looping = false
          Speed = CharConfig.BigFrames
          Columns = 8 }
       BigWalk =
        { Index = 9
          Looping = true
          Speed = CharConfig.BigFrames
          Columns = 8 } |}

let charSprite: SpriteConfig =
    AnimatedSpriteConfig
        { Images = [ smallCharImage; bigCharImage ]
          InitAnimation = CharAnimations.SmallWalk
          Started = false
          Tint = Color.White
          FrameLength = 300L }