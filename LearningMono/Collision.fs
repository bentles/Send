module Collision

open Microsoft.Xna.Framework

let EPSILON = 1e-8f

let clamp value min max =
    if value < min then min
    elif value > max then max
    else value

let inline (<*>) (a: Vector2) (b: Vector2) = Vector2.Multiply(a, b)

//let inline (<*>) (a:Vector2) (b:float32) =
//    Vector2.Multiply(a, b)

//let inline (<*>) (a:Vector2) (b:int) =
//    Vector2.Multiply(a, float32 b)

//let inline (<*>) (a:int) (b:Vector2) =
//    Vector2.Multiply(b, float32 a)

//let inline (<*>) (a:float32) (b:Vector2) =
//    Vector2.Multiply(b, a)

let inline (<+>) (a: Vector2) (b: Vector2) = Vector2.Add(a, b)


type AABB = { Pos: Vector2; Half: Vector2 }

type Hit =
    { Collider: AABB
      Pos: Vector2
      Delta: Vector2
      Normal: Vector2
      Time: float32 }

type Sweep =
    { Hit: Hit option
      Pos: Vector2
      Time: float32 } //default to 1

let intersectSegment (aabb: AABB) (pos: Vector2) (delta: Vector2) paddingX paddingY : Hit option =
    let scaleX = 1.0f / delta.X
    let scaleY = 1.0f / delta.Y

    let signX = float32 (sign (scaleX))
    let signY = float32 (sign (scaleY))

    let nearTimeX = (aabb.Pos.X - signX * (aabb.Half.X + paddingX) - pos.X) * scaleX
    let nearTimeY = (aabb.Pos.Y - signY * (aabb.Half.Y + paddingY) - pos.Y) * scaleY
    let farTimeX = (aabb.Pos.X + signX * (aabb.Half.X + paddingX) - pos.X) * scaleX
    let farTimeY = (aabb.Pos.Y + signY * (aabb.Half.Y + paddingY) - pos.Y) * scaleY

    if nearTimeX > farTimeY || nearTimeY > farTimeX then
        None
    else
        let nearTime = max nearTimeX nearTimeY
        let farTime = min farTimeX farTimeY

        if nearTime >= 1.0f || farTime <= 0f then
            None
        else
            let hittime = clamp nearTime 0f 1f

            let hitNormal =
                if (nearTimeX > nearTimeY) then
                    Vector2(-signX, 0f)
                else
                    Vector2(0f, -signY)

            let hitdelta = Vector2((1.0f - hittime) * -delta.X, (1.0f - hittime) * -delta.Y)
            let hitpos = Vector2(pos.X + delta.X * hittime, pos.Y + delta.Y * hittime)

            Some
                { Collider = aabb
                  Pos = hitpos
                  Delta = hitdelta
                  Normal = hitNormal
                  Time = hittime }

let intersectAABB (aabb: AABB) (box: AABB) : Hit option =
    let dx = box.Pos.X - aabb.Pos.X
    let px = (box.Half.X + aabb.Half.X) - (abs dx)

    if (px <= 0f) then
        None
    else
        let dy = box.Pos.Y - aabb.Pos.Y
        let py = (box.Half.Y + aabb.Half.Y) - (abs dy)

        if (py <= 0f) then
            None
        else

        if
            (px < py)
        then
            let sx = float32 (sign dx)

            Some
                { Time = 0f
                  Collider = aabb
                  Delta = Vector2(px * sx, 0f)
                  Normal = Vector2(sx, 0f)
                  Pos = Vector2(aabb.Pos.X + (aabb.Half.X * sx), box.Pos.Y) }
        else
            let sy = float32 (sign dy)

            Some
                { Time = 0f
                  Collider = aabb
                  Delta = Vector2(0f, py * sy)
                  Normal = Vector2(0f, sy)
                  Pos = Vector2(box.Pos.X, aabb.Pos.Y + (aabb.Half.Y * sy)) }


let sweepAABB (aabb: AABB) (box: AABB) (delta: Vector2) : Sweep =
    if delta.X = 0f && delta.Y = 0f then
        let hit = intersectAABB aabb box

        match hit with
        | Some hit ->
            { Pos = box.Pos
              Hit = Some { hit with Time = 0f }
              Time = 0f }
        | None -> { Pos = box.Pos; Hit = None; Time = 1f }
    else
        let hit = intersectSegment aabb box.Pos delta box.Half.X box.Half.Y

        match hit with
        | Some hit ->
            let sweepTime = clamp (hit.Time - EPSILON) 0f 1f
            let sweepPosX = box.Pos.X + delta.X * sweepTime
            let sweepPosY = box.Pos.Y + delta.Y * sweepTime
            let sweepPos = Vector2(sweepPosX, sweepPosY)
            let direction = Vector2.Multiply(delta, 1f) //don't know how to copy lol
            let direction = Vector2.Normalize(direction)

            let hitPosX =
                clamp (hit.Pos.X + direction.X * box.Half.X) (aabb.Pos.X - aabb.Half.X) (aabb.Pos.X + aabb.Half.X)

            let hitPosY =
                clamp (hit.Pos.Y + direction.Y * box.Half.Y) (aabb.Pos.Y - aabb.Half.Y) (aabb.Pos.Y + aabb.Half.Y)

            let hitPos = Vector2(hitPosX, hitPosY)

            { Hit = Some { hit with Pos = hitPos }
              Pos = sweepPos
              Time = sweepTime }
        | None ->
            let sweepPosX = box.Pos.X + delta.X
            let sweepPosY = box.Pos.Y + delta.Y
            let sweepPos = Vector2(sweepPosX, sweepPosY)
            let sweepTime = 1f

            { Hit = None
              Pos = sweepPos
              Time = sweepTime }

let sweepInto (aabb: AABB) (staticColliders: AABB seq) (delta: Vector2) : Sweep =
    let nearest: Sweep =
        { Time = 1f
          Pos = Vector2(aabb.Pos.X + delta.X, aabb.Pos.Y + delta.Y)
          Hit = None }

    let nearestCollisionFn =
        (fun sweep collider ->
            let newSweep = sweepAABB aabb collider delta
            if sweep.Time < newSweep.Time then sweep else newSweep)

    Seq.fold nearestCollisionFn nearest staticColliders