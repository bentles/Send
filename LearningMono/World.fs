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
      BlockDim: int
      }

let init chunkDim blockDim =
    let chunks =
        Map.ofSeq (
            seq {
                for x in -1 .. 0 do
                    for y in -1 .. 0 -> ((x, y), { Blocks = Array.create (chunkDim * chunkDim) { Type = 0 } })
            }
        )

    { Chunks = chunks; ChunkDim = chunkDim; BlockDim = blockDim }

let drawWorld (model:Model) = 
    colour Color.Red (model.BlockDim - 1, model.BlockDim - 1) (1,2)

    //Viewable.OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->
    //())

let view model =
    [ 
      yield drawWorld model
 ]