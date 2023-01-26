module Config

open Microsoft.Xna.Framework
open Xelmish.Model

//game config
let window = Windowed(1600, 900)
let AcceptableError = 0.001f

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
      Started: bool
      Tint: Color
      FrameLength: int64 }

type SingleSpriteConfig =
    { Image: ImageConfig
      Tint: Color
      FrameLength: int64 }

type SpriteConfig =
    | SingleSpriteConfig of SingleSpriteConfig
    | AnimatedSpriteConfig of AnimatedSpriteConfig

let getTotalRows (config: SpriteConfig) =
    match config with
    | SingleSpriteConfig single -> single.Image.Rows
    | AnimatedSpriteConfig anim -> anim.Images |> List.fold (fun acc img -> acc + img.Rows) 0

let playerConfig =
    { BigMaxVelocity = 2f
      SmallMaxVelocity = 4.5f
      Acc = 30f
      Slow = 40f
      AABBConfig =
        { Half = Vector2(25f, 25f)
          Offset = Vector2(0f, 20f) } }

let bigCharImage =
    { PixelSize = (800, 728)
      Rows = 7
      Columns = 8
      TextureName = "bigChar"
      Offset = Vector2.Zero }

let smallCharImage =
    { PixelSize = (416, 256)
      Rows = 4
      Columns = 8
      TextureName = "smallChar"
      Offset = Vector2(0f, -18f) }

let timerImage =
    { PixelSize = (200, 50)
      Rows = 1
      Columns = 4
      TextureName = "timer"
      Offset = Vector2(0f, 0f) }

let idImage =
    { PixelSize = (50, 150)
      Rows = 3
      Columns = 1
      TextureName = "id"
      Offset = Vector2(0f, 0f) }

let mapImage = { idImage with TextureName = "map" }
let filterImage = { idImage with TextureName = "filter" }

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
