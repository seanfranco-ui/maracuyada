module App.Menu

open System
open System.Threading

//
// Esta linea es para traer los simbolos
// del module App.Utils
//
open App.Utils

type MenuState =
| Active
| Terminated

type Command =
| NewRockSim
| NewMonsterSim
| Exit

type State = {
    MenuState: MenuState
    X: int
    Y: int
    CurSorSelection: int
    CursorX: int
    Commands: (Command * string) array
    RedrawScreen: bool
    Enter : bool
    Option : Command
}

let initialState = {
    MenuState = Active
    X = 20
    Y = Console.BufferHeight/2
    CurSorSelection = 0
    CursorX = 18
    Commands = [|
        NewRockSim,"Simulacion de Roca"
        NewMonsterSim, "Simulacion de Monstruo"
        Exit,"Salir"
    |]
    RedrawScreen = true
    Enter = false
    Option = NewRockSim
}

let drawMenu state =
    state.Commands
    |> Array.iteri (fun i (_,legend) ->
        displayMessage state.X (state.Y+i) ConsoleColor.Cyan legend
    )

    displayMessage state.CursorX (state.Y+state.CurSorSelection) ConsoleColor.Yellow "*"
let redrawScreen state =
    if state.RedrawScreen then
        Console.Clear()
        state |> drawMenu
        {state with RedrawScreen = false}
    else
        state
let lookForEnter key state =
    match key with
    |ConsoleKey.Enter -> {state with Enter = true}
    |_ -> {state with Enter = false}    
let updateMenuKeyboard key state =
    let newState =
        match key with 
        | ConsoleKey.UpArrow -> {state with CurSorSelection = max 0 (state.CurSorSelection-1)}
        | ConsoleKey.DownArrow -> {state with CurSorSelection = min (state.Commands.Length-1) (state.CurSorSelection+1)}
        | ConsoleKey.Enter -> {state with MenuState = Terminated}
        | _ -> state

    if newState <> state then 
        {newState with RedrawScreen = true}
    else
        state

let processKeyboard state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state
        |> updateMenuKeyboard k.Key
        |> lookForEnter k.Key
    else
        state
    
let rec menuState state=
    let stateM =
     processKeyboard state 
     |> redrawScreen
    if stateM.Enter = true then 
         match stateM.CurSorSelection with
         |0 -> {stateM with MenuState = Terminated; Option = NewRockSim } 
         |1 -> 
               {stateM with MenuState = Terminated; Option = NewMonsterSim } 
         |2 -> {stateM with MenuState = Terminated; Option = Exit } 
         |_-> {stateM with MenuState = Terminated; Option = Exit } 
        else
         menuState stateM          
let rec mainLoop state =
    let newState = 
        state
        |> processKeyboard
        |> redrawScreen
        |> menuState
    if newState.MenuState = Active then
        Thread.Sleep 25
        mainLoop newState
    else
     newState    

let mostrar() =
    let oldForeground = Console.ForegroundColor
    Console.CursorVisible <- false

    let opcion= initialState
                        |> mainLoop

    Console.CursorVisible <- true
    Console.ForegroundColor <- oldForeground
    Console.Clear()
    opcion
