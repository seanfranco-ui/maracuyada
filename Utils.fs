module App.Utils
open System
let displayMessage x y color (msg: string) = 
  Console.SetCursorPosition(x,y)
  Console.ForegroundColor <- color
  msg |> Console.Write
let displayMessageRight y (msg:String) color =
     let l = msg.Length
     let start = Console.BufferWidth-l-1
     displayMessage start y color msg