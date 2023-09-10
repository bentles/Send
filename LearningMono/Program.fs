open Elmish
open Xelmish.Model // required for config types used when using program.run
open Xelmish.Viewables // required to get access to helpers like 'colour'
open GameConfig
open Prelude

type Model = { Game: Game.Model
               LevelEditor: LevelEditor.Model 
               TimeElapsed: int64
    }

let init () =
    { Game = Game.init 0L
      LevelEditor = LevelEditor.init () 
      TimeElapsed = 0L
    }, Cmd.none

type Message =
    | WorldMessage of Game.Message
    | Tick of int64

let update message (model: Model) =
    match message with
    | WorldMessage p ->
        let newWorld = Game.update p model.Game
        { model with Game = newWorld }, Cmd.none
    | Tick time -> { model with TimeElapsed = time } , Cmd.none 


let view (model: Model) (dispatch: Message -> unit) =
    [
        OnDraw(fun loadedAssets inputs spriteBatch -> 
            Game.view model.Game (WorldMessage >> dispatch) loadedAssets inputs spriteBatch
        )
        OnUpdate(fun inputs -> 
            Game.inputs inputs (WorldMessage >> dispatch)
            dispatch (Tick inputs.totalGameTime) //TODO: probs don't need this
            if Keyboard.iskeydown Keys.Escape inputs then exit()
            )
    ]

[<EntryPoint>]
let main _ =
    let config: GameConfig =
        { clearColour = Some Colour.CornflowerBlue
          resolution = window
          assetsToLoad =
            [
              //tiles
              PipelineTexture("tile", "./content/sprites/tile/Tile")
              PipelineTexture("facingUp", "./content/sprites/tile/FacingUp")
              PipelineTexture("facingRight", "./content/sprites/tile/FacingRight")
              PipelineTexture("feet", "./content/sprites/tile/Feet")
              PipelineTexture("tile", "./content/sprites/tile/Tile")
              PipelineTexture("void", "./content/sprites/tile/Void")
              PipelineTexture("grass", "./content/sprites/tile/Grass")
              PipelineTexture("wall", "./content/sprites/tile/Wall")
              PipelineTexture("floor", "./content/sprites/tile/Floor")
              PipelineTexture("topWall", "./content/sprites/tile/TopWall")
              PipelineTexture("leftWall", "./content/sprites/tile/LeftWall")
              PipelineTexture("rightWall", "./content/sprites/tile/RightWall")
              PipelineTexture("bottomWall", "./content/sprites/tile/BottomWall")

              //entities
              PipelineTexture("bigChar", "./content/sprites/entity/BigChar")
              PipelineTexture("bigCharAttack", "./content/sprites/entity/BigCharAttack")
              PipelineTexture("smallChar", "./content/sprites/entity/SmallChar")
              PipelineTexture("rock", "./content/sprites/entity/Rock")
              PipelineTexture("id", "./content/sprites/entity/Id")
              PipelineTexture("map", "./content/sprites/entity/Map")
              PipelineTexture("filter", "./content/sprites/entity/Filter")
              PipelineTexture("timer", "./content/sprites/entity/Timer")
              PipelineTexture("nextLevel", "./content/sprites/entity/NextLevel")
              PipelineTexture("button", "./content/sprites/entity/Button")
              PipelineTexture("toggleOff", "./content/sprites/entity/ToggleOff")
              PipelineTexture("toggleOn", "./content/sprites/entity/ToggleOn")
              PipelineTexture("boxOpen", "./content/sprites/entity/BoxOpen")
              PipelineTexture("boxClosed", "./content/sprites/entity/BoxClosed")
              PipelineTexture("unit", "./content/sprites/entity/Unit")
              PipelineTexture("merge", "./content/sprites/entity/Merge")
              PipelineTexture("compare", "./content/sprites/entity/Compare")

              //music
              PipelineMusic("tutorial", "./content/music/tutorial")
              PipelineMusic("pewpew", "./content/music/pewpew")

              //soundfx
              PipelineSound("click", "./content/soundfx/click2")
              PipelineSound("pickUp", "./content/soundfx/pickup")
              PipelineSound("place", "./content/soundfx/putdown")

              //fonts
              PipelineFont("defaultFont", "./content/SourceCodePro") ]
          mouseVisible = true }

    Program.mkProgram init update view 
    |> Xelmish.Program.runGameLoop config

    0
