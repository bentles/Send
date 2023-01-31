open Elmish
open Xelmish.Model // required for config types used when using program.run
open Xelmish.Viewables // required to get access to helpers like 'colour'
open Microsoft.Xna.Framework
open Config
open System


type Model = { World: World.Model
               Levels: WorldConfig list
               CurrentLevel: int
               TimeElapsed: int64
    }

let initLevel (level: WorldConfig) =
    World.init worldConfig

let init () =
    let level1 = worldConfig
    let level2 = worldConfig

    { World = (initLevel level1 0)
      Levels = [level1; level2]
      CurrentLevel = 0
      TimeElapsed = 0
    }, Cmd.none

type Message =
    | WorldMessage of World.Message
    | NextLevel
    | Tick of int64

let update message (model: Model) =
    match message with
    | WorldMessage p ->
        let (newWorld, cmd) = World.update p model.World
        { model with World = newWorld }, Cmd.map WorldMessage cmd
    | NextLevel -> 
        let nextLevel = (model.CurrentLevel + 1) % model.Levels.Length
        { model with CurrentLevel = nextLevel; World = initLevel model.Levels[nextLevel] model.TimeElapsed }, Cmd.none
    | Tick time -> { model with TimeElapsed = time } , Cmd.none


let view (model: Model) (dispatch: Message -> unit) =
    seq { 
      yield! World.view model.World (WorldMessage >> dispatch)

      yield onupdate (fun input -> dispatch (Tick input.totalGameTime))

      //input
      yield onkeydown Keys.N (fun input -> dispatch (NextLevel))
      yield onkeydown Keys.Escape exit } |> Seq.toList

[<EntryPoint>]
let main _ =
    let config: GameConfig =
        { clearColour = Some Colour.CornflowerBlue
          resolution = Windowed(1600, 900)
          assetsToLoad =
            [
              //tiles
              PipelineTexture("tile", "./content/sprites/tile/Tile")
              PipelineTexture("facingUp", "./content/sprites/tile/FacingUp")
              PipelineTexture("facingRight", "./content/sprites/tile/FacingRight")
              PipelineTexture("tile", "./content/sprites/tile/Tile")
              PipelineTexture("grass", "./content/sprites/tile/Grass")

              //entities
              PipelineTexture("bigChar", "./content/sprites/entity/BigChar")
              PipelineTexture("smallChar", "./content/sprites/entity/SmallChar")
              PipelineTexture("rock", "./content/sprites/entity/Rock")
              PipelineTexture("id", "./content/sprites/entity/Id")
              PipelineTexture("map", "./content/sprites/entity/Map")
              PipelineTexture("filter", "./content/sprites/entity/Filter")
              PipelineTexture("timer", "./content/sprites/entity/Timer")

              //fonts
              PipelineFont("defaultFont", "./content/SourceCodePro") ]
          mouseVisible = true }

    Program.mkProgram init update view 
    |> Xelmish.Program.runGameLoop config

    0
