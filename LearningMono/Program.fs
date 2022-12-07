open Elmish
open Xelmish.Model // required for config types used when using program.run
open Xelmish.Viewables // required to get access to helpers like 'colour'
open Microsoft.Xna.Framework
open Config

type Model =
    { Player: Player.Model
      World: World.Model }


let init () =
    { Player = Player.init 100 100 6f 60f 40f charSprite
      World = World.init 20 75 },
    Cmd.none

type Message =
    | PlayerMessage of Player.Message
    | Tick

let update message (model: Model) =
    match message with
    | PlayerMessage p ->
        let (newPlayer, cmd) = Player.update p model.Player
        { model with Player = newPlayer }, Cmd.map PlayerMessage cmd
    | Tick -> model, Cmd.none


let view (model: Model) (dispatch: Message -> unit) =
    [ yield! World.view model.World
      yield! Player.view model.Player (PlayerMessage >> dispatch)
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
              PipelineFont("defaultFont", "./content/SourceCodePro") ]
          mouseVisible = false }

    Program.mkProgram init update view
    |> Xelmish.Program.runGameLoop config

    0
