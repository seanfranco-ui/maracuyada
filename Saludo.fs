module App.Saludo

open System
open System.Threading

open Utils

type ProgramState = 
| Running
| Terminated


type KeyboardHelper =
| Active
| WithData

type KeyBoardState = {
    KeyboardHelper: KeyboardHelper
    data: string
    x: int
    y: int

}

let initialKeyboard = {
    KeyboardHelper = Active
    data = ""
    x = 0
    y = 0

}

type State = {
    ProgramState: ProgramState
    Tick: int
    Clock: int
    RedrawScreen: bool
    KeyboardState: KeyBoardState
    Mensaje : string
}

let initialState = {
    ProgramState = Running
    Tick = -1
    Clock = 0
    RedrawScreen = true
    KeyboardState = initialKeyboard
    Mensaje = "Entra tu Nombre: "
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
let Enter key state =
    match key with 
    | ConsoleKey.Enter -> {state with KeyboardState.KeyboardHelper = WithData}
    | _ -> state    
let escribirNombre (key: ConsoleKeyInfo) state  =
     let y  = [state.KeyboardState.data]
     let x = int key.KeyChar
     if (key.Key = ConsoleKey.Backspace) = false  then
      let ab = 
       if (x>=48 && x<=57) || (x>=65 && x<=90) || (x>=97 && x<=122) || x = 32 then [
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
      {state with KeyboardState.data = r2; RedrawScreen = true;}
       
    




let redrawClock state =
    displayMessageRight 0  $"{state.Clock}" ConsoleColor.Yellow


let mostrarSaludo estado =
    match estado.KeyboardState.KeyboardHelper with 
    | Active ->
        displayMessage estado.KeyboardState.x  estado.KeyboardState.y ConsoleColor.Yellow estado.Mensaje
        displayMessage (estado.KeyboardState.x+estado.Mensaje.Length) estado.KeyboardState.y ConsoleColor.Yellow estado.KeyboardState.data
        displayMessage (estado.KeyboardState.data.Length+estado.Mensaje.Length) estado.KeyboardState.y ConsoleColor.Yellow "☠️"
    | WithData ->
        displayMessage estado.KeyboardState.x  estado.KeyboardState.y ConsoleColor.Cyan $"Hola {estado.KeyboardState.data}"

let pipeline = [|
    updateClock
    updateTick
|]
let alternativeBoard = [|escribirNombre|]
let mainLoop  =
        createMainLoop 
         pipeline 
         (fun s -> s.ProgramState = Running) 
         [| Enter;updateSaludoKeyboard|]
         [| redrawClock;mostrarSaludo|]
         (fun s -> s.RedrawScreen)
         (fun s -> {s with RedrawScreen=false})
         alternativeBoard

let mostrar() =
    Console.Clear()
    Console.CursorVisible <- false

    initialState 
    |> mainLoop
    |> ignore
    
    Console.CursorVisible <- true
    Console.Clear()
    