module World

open Xelmish.Model
open Xelmish.Viewables
open Microsoft.Xna.Framework
open Config
open Elmish

let Empty = 0
let Grass = 1

[<Struct>]
type Block = { Type: int }

type Chunk = { Pos: int * int; Blocks: Block[] }

type Model =
    { Chunks: Map<int * int, Chunk>
      ChunkBlockLength: int
      BlockWidth: int

      //player and camera
      Player: Player.Model
      CameraPos: Vector2 }

type Message =
    | PlayerMessage of Player.Message
    | PhysicsTick of time: int64

let updateCameraPos (playerPos: Vector2) (oldCamPos: Vector2) : Vector2 =
    let diff = Vector2.Subtract(playerPos, oldCamPos)
    let halfDiff = Vector2.Multiply(diff, 0.25f)

    if halfDiff.LengthSquared() < 0.5f then
        playerPos
    else
        oldCamPos + halfDiff

let halfScreenOffset (camPos: Vector2) : Vector2 =
    Vector2.Subtract(camPos, Vector2(800f, 450f))

let chunksToRender (cameraPos: Vector2) (chunks: Map<int * int, Chunk>) : Chunk seq =
    seq {
        yield! (Map.values chunks) //TODO: logic about which chunks to render based on the camera position
    }

let init (worldConfig: WorldConfig) =
    let blocks = worldConfig.ChunkBlockLength * worldConfig.ChunkBlockLength

    let chunks =
        Map.ofSeq (
            seq {
                for X in -1 .. 0 do
                    for y in -1 .. 0 ->
                        let pos = (x, y)

                        (pos,
                         { Pos = pos
                           Blocks = Array.create (int blocks) { Type = 0 } })
            }
        )

    { Chunks = chunks
      ChunkBlockLength = worldConfig.ChunkBlockLength
      BlockWidth = worldConfig.BlockWidth
      Player = Player.init 0 0 playerConfig charSprite
      CameraPos = Vector2(0f, -0f) }


let update (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | PlayerMessage playerMsg ->
        let (newPlayerModel, playerCommand) = Player.update playerMsg model.Player
        { model with Player = newPlayerModel }, Cmd.map PlayerMessage playerCommand
    | PhysicsTick time ->
        let newCameraPos = updateCameraPos model.Player.Pos model.CameraPos
        { model with CameraPos = newCameraPos }, Cmd.none

let drawWorld (model: Model) =
    OnDraw(fun loadedAssets _ (spriteBatch: SpriteBatch) ->

        let blockWidth = model.BlockWidth
        let empty = loadedAssets.textures["tile"]
        let grass = loadedAssets.textures["grass"]
        let chunkSize = blockWidth * model.ChunkBlockLength

        let sourceRect = rect 0 0 blockWidth blockWidth

        let cameraOffset = -(halfScreenOffset model.CameraPos)

        model.Chunks
        |> Map.iter (fun (x, y) chunk ->
            chunk.Blocks
            |> Array.iteri (fun i block ->

                let texture =
                    match block.Type with
                    | 1 -> grass
                    | _ -> empty

                let startX = x * chunkSize
                let startY = y * chunkSize

                let xBlockOffSet = (i % model.ChunkBlockLength) * blockWidth
                let yBlockOffSet = (i / model.ChunkBlockLength) * blockWidth

                let actualX = startX + xBlockOffSet + int (cameraOffset.X)
                let actualY = startY + yBlockOffSet + int (cameraOffset.Y)

                spriteBatch.Draw(texture, Rectangle(actualX, actualY, sourceRect.Width, sourceRect.Height), Color.White))))

let view model (dispatch: Message -> unit) =
    [ yield onupdate (fun input -> dispatch (PhysicsTick input.totalGameTime))

      yield drawWorld model
      yield! Player.view model.Player (halfScreenOffset model.CameraPos) (PlayerMessage >> dispatch) ]
