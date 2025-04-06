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

let memErr x =
    Declare "ptr" />
    Alloc("ptr", Num x) />
    Declare "x" />
    ("x" .<-. Num x) />
    While(Num 0 .<. Var "x",
            MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x") />
            ("x" .<-. Var "x" .-. Num 1)) />
    ("x" .<-. Num (x - 1)) />
    Declare "result" />
    ("result" .<-. Num 1) />
    While(Num 0 .<=. Var "x",
            ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x")) />
            ("x"      .<-. Var "x" .-. Num 1)) />
    Free(Var "ptr", Num (x + 1))

printf("%A \n") ((stmntEval (memErr 5) >>>= getVar "result") |>evalState (mkState 100 None Map.empty))