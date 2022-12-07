module World

open Xelmish.Model
open Xelmish.Viewables
open Microsoft.Xna.Framework

let Empty = 0

[<Struct>]
type Block = { Type: int }

type Chunk = { Blocks: Block[] }

type Model =
    { Chunks: Map<int * int, Chunk>
      ChunkDim: int
      BlockDim: int }

let init chunkDim blockDim =
    let chunks =
        Map.ofSeq (
            seq {
                for x in -1 .. 0 do
                    for y in -1 .. 0 -> ((x, y), { Blocks = Array.create (chunkDim * chunkDim) { Type = 0 } })
            }
        )

    { Chunks = chunks
      ChunkDim = chunkDim
      BlockDim = blockDim }

let drawWorld (model: Model) =
    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->

        let blockSize = 75
        let texture = loadedAssets.textures["tile"]
        let chunkSize = blockSize * model.ChunkDim

        let sourceRect = rect 0 0 75 75

        model.Chunks
        |> Map.iter (fun (x, y) chunk ->
            chunk.Blocks
            |> Array.iteri (fun i block ->
                let startX = x * chunkSize
                let startY = y * chunkSize

                let xOffSet = (i % model.ChunkDim) * blockSize
                let yOffSet = (i / model.ChunkDim) * blockSize

                spriteBatch.Draw(
                    texture,
                    Rectangle(startX + xOffSet, startY + yOffSet, sourceRect.Width, sourceRect.Height),
                    Color.White
                ))))

let view model = [ yield drawWorld model ]
