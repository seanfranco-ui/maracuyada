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

let lookForEnter key state=
     match key with
     |ConsoleKey.Enter -> {state with enter = true; rockY = 0}
     |_ -> {state with enter = false}
let lookForESC key state =
    match key with
    |ConsoleKey.Escape -> {state with ProgramState = Terminated}
    |_ -> state
let displayRock state =
    displayMessage state.rockX state.rockY  ConsoleColor.Green "*"
    state
let updateRockPos state=
     if state.Tick <> 0  && state.Tick % 5 = 0 then
      {state with rockY = min (height-1) (state.rockY + 1 ); redrawScreen = true }
     else
      state    

let processKeyboard state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state
        |> lookForEnter k.Key
        |> lookForESC k.Key
    else
        state     
let redrawScreen state =
    if state.redrawScreen then 
        Console.Clear()
        state
        |> displayRock
        |> fun s -> {s with redrawScreen=false}
    else
     state
let rec mainLoop state =
    let newState =
     state
     |> updateTick
     |> processKeyboard
     |> updateRockPos
     |> redrawScreen 
    if newState.ProgramState = Active then
     Thread.Sleep 25
     mainLoop newState              
let Rock() =
 initialState
 |> mainLoop 
 Console.ForegroundColor <- oldcolor
 Console.BackgroundColor <- oldBlack
 Console.Clear()      