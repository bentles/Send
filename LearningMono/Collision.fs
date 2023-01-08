module Collision

open Microsoft.Xna.Framework
open System
open Config
open Xelmish.Viewables

let EPSILON = 1e-8f

let clamp value min max =
    if value < min then min
    elif value > max then max
    else value

type AABB = { Pos: Vector2; Half: Vector2 }

type CollisionInfo =
    {
      //collision
      Half: Vector2
      Offset: Vector2 }

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

let renderAABB (aabb: AABB) (cameraPos: Vector2) =
    image
        "tile"
        Color.Red
        (int (aabb.Half.X * 2f), int (aabb.Half.Y * 2f))
        (int (aabb.Pos.X - aabb.Half.X - cameraPos.X), int (aabb.Pos.Y - aabb.Half.Y - cameraPos.Y))

let collider (pos: Vector2) (collisionInfo: CollisionInfo) : AABB =
    { Pos = pos + collisionInfo.Offset
      Half = collisionInfo.Half }

let intersectSegment (aabb: AABB) (pos: Vector2) (delta: Vector2) paddingX paddingY : Hit option =
    let scaleX = 1.0f / delta.X
    let scaleY = 1.0f / delta.Y

    let signX = float32 (sign (scaleX))
    let signY = float32 (sign (scaleY))

    let nearTimeX = (aabb.Pos.X - signX * (aabb.Half.X + paddingX) - pos.X) * scaleX
    let nearTimeY = (aabb.Pos.Y - signY * (aabb.Half.Y + paddingY) - pos.Y) * scaleY

    let farTimeX = (aabb.Pos.X + signX * (aabb.Half.X + paddingX) - pos.X) * scaleX
    let farTimeY = (aabb.Pos.Y + signY * (aabb.Half.Y + paddingY) - pos.Y) * scaleY

    //yeah so i went case by case and found a workaround for this sticking problem.... :(
    //at least it works
    let farTimeX = if Single.IsNaN(farTimeX) then -infinityf else farTimeX
    let farTimeY = if Single.IsNaN(farTimeY) then -infinityf else farTimeY

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
        else if (px < py) then
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
            let direction = delta * 1f //don't know how to copy lol
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
        (fun nearest collider ->
            let sweep = sweepAABB collider aabb delta
            if sweep.Time < nearest.Time then sweep else nearest)

    Seq.fold nearestCollisionFn nearest staticColliders

let collide pos oldPos colInfo obstacles =
    let sweepIntoWithOffset pos oldPos obstacles =
        let deltaPos = pos - oldPos
        let sweepResult = sweepInto (collider oldPos colInfo) obstacles deltaPos
        let result = { sweepResult with Pos = sweepResult.Pos - colInfo.Offset }

        //collision distance should be <= unadjusted distance
        assert ((result.Pos - oldPos).Length() <= deltaPos.Length() + AcceptableError)
        result


    if Seq.isEmpty obstacles then
        pos
    else
        let sweep1 = sweepIntoWithOffset pos oldPos obstacles

        match sweep1.Hit with
        | Some hit ->
            let movementIntoAABB = pos - sweep1.Pos
            let vectorOut = (hit.Normal * hit.Normal) * movementIntoAABB //grab the component that points out
            let deltaParallel = movementIntoAABB - vectorOut //calc component along the surface

            if deltaParallel = Vector2.Zero then
                sweep1.Pos
            else
                // collide again
                let sweep2 = sweepIntoWithOffset (sweep1.Pos + deltaParallel) sweep1.Pos obstacles

                match sweep2.Hit with
                | Some hit2 -> sweep2.Pos
                | None -> sweep1.Pos + deltaParallel

        | None -> pos