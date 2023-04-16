﻿module GameConfig

open Xelmish.Model
open Microsoft.Xna.Framework

let window = Windowed(1600, 900)//FullScreen(1920, 1080)
let AcceptableError = 0.001f
let DepthFactor = 0.0001f;
let DepthSubFactor = 0.00001f;

let depthConfig = {|
    Tile = 0f;
    Target = 0.1f;
    Entities_And_Player = 0.2f;
    Emitting = 0.3f;
    Debug = 1f;
|}

let worldConfig = {| TileWidth = 50; ShowCollisions = false |}

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