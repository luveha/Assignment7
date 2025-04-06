// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State
open Interpreter.Language
open Interpreter.Memory
open Interpreter.StateMonad

//let runProgram prog =
//    (declare "x" >>>= alloc "y" 10 >>>= arithEval (MemRead(Var "x") .+. Num 42)) |> evalState (CInterpreter.State.mkState 100 None Map.empty)
// Uncomment the program you want to run

//runProgram guessANumber
printf("%A \n") ((declare "x" >>>= alloc "x" 10 >>>= getVar "x" >>= (fun ptr -> setMem ptr 42 >>>= free (ptr + 1) 5 >>>= getMem ptr)) |> evalState (mkState 100 None Map.empty))