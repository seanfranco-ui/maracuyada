// For more information see https://aka.ms/fsharp-console-apps

open App.Rock
open System
open App.Menu
open App.Types

mostrar()
|> fun e -> match e.Option with
                   |NewRockSim -> Rock()
                   |NewMonsterSim -> Console.Clear()
                                     Console.WriteLine"Opcion no implementada cargando nuevo juego"
                   |Exit -> ()                  
                    
