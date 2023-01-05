open Elmish
open Xelmish.Model // required for config types used when using program.run
open Xelmish.Viewables // required to get access to helpers like 'colour'
open Microsoft.Xna.Framework
open Config


type Model = { World: World.Model
               Levels: WorldConfig list
               CurrentLevel: int
    }

let initLevel (level: WorldConfig) =
    World.init worldConfig

let init () =
    let level1 = worldConfig
    let level2 = worldConfig

    { World = (initLevel level1)
      Levels = [level1; level2]
      CurrentLevel = 0
    }, Cmd.none

type Message =
    | WorldMessage of World.Message
    | NextLevel
    | Tick

let update message (model: Model) =
    match message with
    | WorldMessage p ->
        let (newWorld, cmd) = World.update p model.World
        { model with World = newWorld }, Cmd.map WorldMessage cmd
    | NextLevel -> 
        let nextLevel = (model.CurrentLevel + 1) % model.Levels.Length
        { model with CurrentLevel = nextLevel; World = initLevel model.Levels[nextLevel] }, Cmd.none
    | Tick -> model, Cmd.none


let view (model: Model) (dispatch: Message -> unit) =
    [ yield! World.view model.World (WorldMessage >> dispatch)

      //input
      yield onkeydown Keys.N (fun _ -> dispatch (NextLevel))
      yield onkeydown Keys.Escape exit ]

[<EntryPoint>]
let main _ =
    let config: GameConfig =
        { clearColour = Some Colour.CornflowerBlue
          resolution = Windowed(1600, 900)
          assetsToLoad =
            [
              //tiles
              PipelineTexture("tile", "./content/sprites/tile/Tile")
              PipelineTexture("grass", "./content/sprites/tile/Grass")

              //entities
              PipelineTexture("bigChar", "./content/sprites/entity/BigChar")
              PipelineTexture("smallChar", "./content/sprites/entity/SmallChar")
              PipelineTexture("rock", "./content/sprites/entity/Rock")
              PipelineTexture("observer", "./content/sprites/entity/Observer")
              PipelineTexture("timer", "./content/sprites/entity/Timer")

              //fonts
              PipelineFont("defaultFont", "./content/SourceCodePro") ]
          mouseVisible = false }

    Program.mkProgram init update view |> Xelmish.Program.runGameLoop config

    0
