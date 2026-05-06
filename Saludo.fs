module App.Saludo

open System
open System.Threading

open Utils

type ProgramState = 
| Running
| Terminated


type KeyboardHelper =
| Inactive
| Active
| WithData

type KeyBoardState = {
    KeyboardHelper: KeyboardHelper
    data: string
    x: int
    y: int
    CursorX: int
}

let initialKeyboard = {
    KeyboardHelper = Inactive
    data = ""
    x = 0
    y = 0
    CursorX = 0
}

type State = {
    ProgramState: ProgramState
    Tick: int
    Clock: int
    RedrawScreen: bool
    KeyboardState: KeyBoardState
}

let initialState = {
    ProgramState = Running
    Tick = -1
    Clock = 0
    RedrawScreen = true
    KeyboardState = initialKeyboard
}

let updateTick state =
    {state with Tick = state.Tick+1}

let updateClock state =
    if state.Tick <> 0 && state.Tick % 40 = 0 then 
        {state with Clock=state.Clock+1;RedrawScreen=true}
    else
        state

let updateSaludoKeyboard key state =
    match key with 
    | ConsoleKey.Escape -> {state with ProgramState=Terminated}
    | _ -> state
let escribirNombre (key: ConsoleKeyInfo) state  =
     let y  = [state.KeyboardState.data]
     let x = int key.KeyChar
     if (key.Key = ConsoleKey.Backspace) = false  then
      let ab= 
       if (x>=48 && x<=57) || (x>=65 && x<=90) || (x>=97 && x<=122) then [
        key.KeyChar
        |> string
        ]
       else
        [""] 
      let r : string seq = Seq.append y ab 
      {state with KeyboardState.data = r |> Seq.reduce (fun a b -> a + b); RedrawScreen = true} 
     else
      let lenght = state.KeyboardState.data.Length - 1
      let r2 =   state.KeyboardState.data.Substring(0, max 0 lenght)
      {state with KeyboardState.data = r2; RedrawScreen = true; KeyboardState.CursorX = state.KeyboardState.CursorX - 1 }
       
    
    

let processKeyboard state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state
        |> updateSaludoKeyboard k.Key
        |> escribirNombre k
    else
        state

let redrawClock state =
    displayMessageRight 0  $"{state.Clock}" ConsoleColor.Yellow
    state


let redrawMensaje (state:State) =
    displayMessage 0 15 ConsoleColor.Cyan "Entra tu nombre: "
    displayMessage state.KeyboardState.x state.KeyboardState.y ConsoleColor.White state.KeyboardState.data
    displayMessage state.KeyboardState.CursorX state.KeyboardState.y ConsoleColor.DarkBlue "|" 
    let helper = {
        KeyboardHelper = Active
        data = state.KeyboardState.data
        x = 18
        y = 15
        CursorX = 18 + (state.KeyboardState.data.Length)
    }
    {state with KeyboardState = helper; RedrawScreen = true}

let redrawScreen state =
    if state.RedrawScreen then 
        Console.Clear()
        state
        |> redrawClock
        |> redrawMensaje
        |> fun s ->        
        {s with RedrawScreen = false}
    else
        state

let rec mainLoop state =
    let newState =
        state
        |> updateTick
        |> updateClock
        |> processKeyboard
        |> redrawScreen
    if newState.ProgramState <> Terminated then 
        Thread.Sleep 25
        mainLoop newState

let mostrar() =
    Console.Clear()
    Console.CursorVisible <- false

    initialState 
    |> mainLoop

    Console.CursorVisible <- true
    Console.Clear()