module App.Utils
open System
open System.Threading
let createMainLoop pipeline isProgrammingRunning keyboardPipeline  drawPipeline needToRedraw clearRedraw AlternativeKeyboard =
  
    let processKeyboard (state:'State) =
        if Console.KeyAvailable then 
            let k = Console.ReadKey true
            let a =keyboardPipeline
                            |> Array.fold (fun acc f -> acc |> f k.Key) state
            AlternativeKeyboard
            |> Array.fold (fun acc f -> acc |> f k) a
        else
            state
    

    let redrawScreen (state:'State) =
        if needToRedraw state then 
            Console.Clear()
            drawPipeline
            |> Array.iter (fun f -> f state)
            clearRedraw state
        else
            state

    let rec mainLoop (state:'State) =
        
        pipeline
        |> Array.fold ( fun acc f -> f acc) state
        |> processKeyboard
        |> redrawScreen
        |> fun newState ->
            if isProgrammingRunning newState then 
                Thread.Sleep 25
                newState |> mainLoop
            else
                newState
    
    mainLoop
let displayMessage x y color (msg: string) = 
  Console.SetCursorPosition(x,y)
  Console.ForegroundColor <- color
  msg |> Console.Write
let displayMessageRight y (msg:String) color =
     let l = msg.Length
     let start = Console.BufferWidth-l-1
     displayMessage start y color msg