module App.Rock
open App.Utils
open System
open System.Threading
type ProgramState =
 |Active
 |Terminated
type RockSpace = {
  rockX : int
  rockY : int
  Tick : int
  redrawScreen : bool
  enter : bool
  ProgramState : ProgramState
  }
let height = Console.BufferHeight
let widht = Console.BufferWidth
let oldcolor = Console.ForegroundColor
let oldBlack = Console.BackgroundColor
let initialState = {
    rockX = 2
    rockY = 0
    Tick = 0
    redrawScreen = true
    enter = false
    ProgramState = Active
}  
let updateTick state =
   {state with Tick = state.Tick+1}

let updateRockKeyboard key state =
    let newState =
        match key with 
        | ConsoleKey.Enter ->
            {state with rockY=0}
        | ConsoleKey.Escape ->
            {state with ProgramState = Terminated}
        | _ -> state

    if newState <> state then 
        {newState with redrawScreen =true}
    else
        state
let displayRock state =
    displayMessage state.rockX state.rockY  ConsoleColor.Green "*"
    
let updateRockPos state=
     if state.Tick <> 0  && state.Tick % 5 = 0 then
      {state with rockY = min (height-1) (state.rockY + 1 ); redrawScreen = true }
     else
      state    

let pipeline =[|
    updateTick
    updateRockPos
|]     
let alternativeBoard = [||]
let mainLoop =
        createMainLoop 
         pipeline 
         (fun s -> s.ProgramState = Active) 
         [| updateRockKeyboard|]
         [| displayRock|]
         (fun s -> s.redrawScreen)
         (fun s -> {s with redrawScreen=false})
         alternativeBoard
let Rock() =
 initialState
 |> mainLoop 
 |> ignore
 Console.ForegroundColor <- oldcolor
 Console.BackgroundColor <- oldBlack
 Console.Clear()      