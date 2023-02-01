[<RequireQualifiedAccess>]
module Player

open Microsoft.Xna.Framework
open Prelude
open Collision
open Level
open Entity
open GameConfig
open PlayerConfig

type State =
    | Small of bool
    | Growing
    | Shrinking

type Model =
    { SpriteInfo: Sprite.Model
      CharacterState: State

      Input: Vector2
      XInputTimeAndDir: int64 * float32
      YInputTimeAndDir: int64 * float32
      
      MovementFrozen: bool
      ArrowsControlPlacement: bool

      Carrying: Entity.Model list
      Target: Vector2
      PlacementFacing: Facing

      //physics
      Facing: Vector2
      Pos: Vector2
      Acc: float32
      MaxVelocity: float32
      Friction: float32
      Vel: Vector2
      IsMoving: bool

      CollisionInfo: CollisionInfo }

let init x y (playerConfig: PlayerConfig) (spriteConfig: SpriteConfig) time =
    let p = Vector2(float32 x, float32 y)

    { SpriteInfo = Sprite.init p time spriteConfig None None
      CharacterState = Small true
      Input = Vector2.Zero
      XInputTimeAndDir = -1000, 1f
      YInputTimeAndDir = 0, 0f
      PlacementFacing = FacingRight

      MovementFrozen = false
      ArrowsControlPlacement = false

      Carrying =
          [ Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver (Filter Rock)) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight
            Entity.initNoCollider (buildObserver Id) p time FacingRight ]
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
          Offset = playerConfig.AABBConfig.Pos } }


let getPlayerPickupLimit (characterState: State) =
    match characterState with
    | Small true -> 15
    | Small false -> 15
    | _ -> 0


type Message =
    | Input of dir: Vector2
    | TransformCharacter
    | FreezeMovement of bool
    | ArrowsControlPlacement of bool
    | RotatePlacement of clockwise: bool
    | PlayerPhysicsTick of info: PhysicsInfo
    | SpriteMessage of Sprite.Message
    | CarryingMessage of Sprite.Message