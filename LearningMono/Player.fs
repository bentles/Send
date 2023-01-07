module Player

open Microsoft.Xna.Framework
open Config
open Collision


type CharacterState =
    | Small of bool
    | Growing
    | Shrinking



type PlayerModel =
    { SpriteInfo: Sprite.Model
      CharacterState: CharacterState

      Input: Vector2
      XInputTimeAndDir: int64 * float32
      YInputTimeAndDir: int64 * float32
      
      Holding: bool

      Carrying: Entity.Model list
      Target: Vector2

      //physics
      Facing: Vector2
      Pos: Vector2
      Acc: float32
      MaxVelocity: float32
      Friction: float32
      Vel: Vector2
      IsMoving: bool

      CollisionInfo: CollisionInfo }

let initPlayer x y (playerConfig: PlayerConfig) (spriteConfig: SpriteConfig) time =
    let p = Vector2(float32 x, float32 y)

    { SpriteInfo = Sprite.init p time spriteConfig
      CharacterState = Small true
      Input = Vector2.Zero
      XInputTimeAndDir = -1000, 1f
      YInputTimeAndDir = 0, 0f

      Holding = false

      Carrying =
          [ Entity.initNoCollider Entity.Observer p time
            Entity.initNoCollider Entity.Observer p time
            Entity.initNoCollider Entity.Timer p time
            Entity.initNoCollider Entity.Timer p time]
      Facing = Vector2(1f, 0f)
      Target = p + 60f * Vector2(1f, 0f)
      Pos = p
      MaxVelocity = playerConfig.SmallMaxVelocity
      Acc = playerConfig.Acc
      Friction = playerConfig.Slow
      Vel = Vector2.Zero
      IsMoving = false
      CollisionInfo =
        { Half = playerConfig.AABBConfig.Half
          Offset = playerConfig.AABBConfig.Offset } }

type PhysicsInfo =
    { Time: int64
      PossibleObstacles: AABB seq
      Dt: float32 }

type PlayerMessage =
    | Input of dir: Vector2
    | TransformCharacter
    | Hold of bool
    | PlayerPhysicsTick of info: PhysicsInfo
    | SpriteMessage of Sprite.Message
    | CarryingMessage of Sprite.Message