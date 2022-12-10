open Elmish
open Xelmish.Model // required for config types used when using program.run
open Xelmish.Viewables // required to get access to helpers like 'colour'
open Microsoft.Xna.Framework
open Config

type Model = { World: World.Model }


let init () = { World = World.init 20 75 }, Cmd.none

type Message =
    | WorldMessage of World.Message
    | Tick

let update message (model: Model) =
    match message with
    | WorldMessage p ->
        let (newWorld, cmd) = World.update p model.World
        { model with World = newWorld }, Cmd.map WorldMessage cmd
    | Tick -> model, Cmd.none


let view (model: Model) (dispatch: Message -> unit) =
    [ yield! World.view model.World (WorldMessage >> dispatch)
      yield onkeydown Keys.Escape exit ]

[<EntryPoint>]
let main _ =
    let config: GameConfig =
        { clearColour = Some Colour.CornflowerBlue
          resolution = Windowed(1600, 900)
          assetsToLoad =
            [ PipelineTexture("bigChar", "./content/sprites/BigChar")
              PipelineTexture("smallChar", "./content/sprites/SmallChar")
              PipelineTexture("tile", "./content/sprites/Tile")              
              PipelineTexture("grass", "./content/sprites/Grass")
              PipelineFont("defaultFont", "./content/SourceCodePro") ]
          mouseVisible = false }

    Program.mkProgram init update view |> Xelmish.Program.runGameLoop config

    0
