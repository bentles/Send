﻿module PlayerConfig

open Prelude
open Microsoft.Xna.Framework
open GameConfig

// player config

type PlayerConfig =
    { BigMaxVelocity: float32
      SmallMaxVelocity: float32
      MultiPlaceDelayMs: int64
      Acc: float32
      Slow: float32
      AABBConfig: AABB }

let playerConfig =
    { BigMaxVelocity = 2f
      SmallMaxVelocity = 5.5f
      MultiPlaceDelayMs = 250
      Acc = 30f
      Slow = 40f
      AABBConfig =
        { Half = Vector2(24f, 24f)
          Pos = Vector2(0f, 0f) } }

let bigCharImage =
    { SpriteSize = (100, 104)
      Rows = 7
      Columns = 8
      TextureName = "bigChar"
      Offset = Vector2(0f, 27f) }

let bigCharAttackImage =
    { SpriteSize = (120, 114)
      Rows = 3
      Columns = 9
      TextureName = "bigCharAttack"
      Offset = Vector2(-8f, 32f) }

let smallCharImage =
    { SpriteSize = (52, 62)
      Rows = 4
      Columns = 8
      TextureName = "smallChar"
      Offset = Vector2(0f, 6f) }

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
          Columns = 8 }
       BigAttack =
        { Index = 11
          Looping = false
          Speed = 200
          Columns = 9 } |}

let charSprite: SpriteConfig =
    AnimatedSpriteConfig
        { Images = [ smallCharImage; bigCharImage; bigCharAttackImage ]
          InitAnimation = CharAnimations.SmallWalk
          Started = false
          Tint = Color.White
          FrameLength = 300L }

// how much time to consider an old input for to see if the player is trying to
// face diagonally since both fingers come off the keyboard at fractionally different times :(
let diagonalReleaseDelay = 100L
