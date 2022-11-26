open Elmish
open Xelmish.Model // required for config types used when using program.run
open Xelmish.Viewables // required to get access to helpers like 'colour'
open Microsoft.Xna.Framework
open Config

type Model = { player: Player.Model }


let init () =
    { player = Player.init 100 100 6f 60f 40f Player.SmallChar }, Cmd.none

type Message =
    | PlayerMessage of Player.Message
    | Tick

let update message (model: Model) =
    match message with
    | PlayerMessage p ->
        let (newPlayer, cmd) = Player.update p model.player
        { model with player = newPlayer }, Cmd.map PlayerMessage cmd
    | Tick -> model, Cmd.none


let view (model: Model) (dispatch: Message -> unit) =
    [ yield! Player.view model.player (PlayerMessage >> dispatch)

      yield onkeydown Keys.Escape exit ]

[<EntryPoint>]
let main _ =
    let config: GameConfig =
        { clearColour = Some Colour.CornflowerBlue
          resolution = Windowed(1600, 900)
          assetsToLoad =
            [ PipelineTexture("bigChar", "./content/sprites/BigChar")
              PipelineTexture("smallChar", "./content/sprites/SmallChar")
              PipelineFont("defaultFont", "./content/SourceCodePro") ]
          mouseVisible = false }

    Program.mkProgram init update view
    |> Program.withConsoleTrace
    |> Xelmish.Program.runGameLoop config

    0
