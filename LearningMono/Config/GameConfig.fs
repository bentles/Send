module GameConfig

open Xelmish.Model
open Microsoft.Xna.Framework

let window = Windowed(1600, 900)//FullScreen(1920, 1080)

[<RequireQualifiedAccess>]
module DepthConfig =    
  let DepthFactor = 0.0001f;
  let DepthSubFactor = 0.00001f;

  let Tile = 0f;
  let Target = 0.1f;
  let Entities_And_Player = 0.2f;
  let Emitting = 0.3f;
  let Debug = 1f;
    
[<RequireQualifiedAccess>]
module WorldConfig = 
  let TicksPerReactiveUpdate = 3L //i.e. 20fps since game runs at 60fps
  let AcceptableError = 0.001f
  let TileWidth = 50; 
  let HalfTileWidth = 25; 
  let ShowCollisions = false 

let halfTile = Vector2(float32 WorldConfig.HalfTileWidth, float32 WorldConfig.HalfTileWidth)

type ImageConfig =
    { SpriteSize: struct (int * int)
      Rows: int
      Columns: int
      Offset: Vector2
      TextureName: string }

type AnimationConfig =
    { Index: int
      Looping: bool
      Speed: int
      Columns: int }

let imageSpriteConfig =
    { Looping = false
      Speed = 0
      Index = 0
      Columns = 1 }

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