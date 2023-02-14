open Elmish
open Xelmish.Model // required for config types used when using program.run
open Xelmish.Viewables // required to get access to helpers like 'colour'
open GameConfig
open Prelude

type Model = { World: World.Model
               TimeElapsed: int64
    }

let init () =
    { World = (World.init 0)
      TimeElapsed = 0
    }, Cmd.none

type Message =
    | WorldMessage of World.Message
    | Tick of int64

let update message (model: Model) =
    match message with
    | WorldMessage p ->
        let (newWorld) = World.update p model.World
        { model with World = newWorld }, Cmd.none
    | Tick time -> { model with TimeElapsed = time } , Cmd.none 


let view (model: Model) (dispatch: Message -> unit) =
    [
        OnDraw(fun loadedAssets inputs spriteBatch -> 
            World.draw model.World (WorldMessage >> dispatch) loadedAssets inputs spriteBatch
        )
        OnUpdate(fun inputs -> 
            World.inputs inputs (WorldMessage >> dispatch)
            dispatch (Tick inputs.totalGameTime) //TODO: probs don't need this
            if KeyBoard.iskeydown Keys.Escape inputs then exit()
            )
    ]

    // seq { 
    //   yield! World.view model.World (WorldMessage >> dispatch)

    //   yield onupdate (fun input -> dispatch (Tick input.totalGameTime))

    //   //input
    //   yield onkeydown Keys.Escape exit } |> Seq.toList

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
              PipelineTexture("wall", "./content/sprites/tile/Wall")
              PipelineTexture("topWall", "./content/sprites/tile/TopWall")
              PipelineTexture("leftWall", "./content/sprites/tile/LeftWall")
              PipelineTexture("rightWall", "./content/sprites/tile/RightWall")
              PipelineTexture("bottomWall", "./content/sprites/tile/BottomWall")

              //entities
              PipelineTexture("bigChar", "./content/sprites/entity/BigChar")
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

              //music
              PipelineMusic("tutorial", "./content/music/tutorial")

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
