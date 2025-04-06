module Interpreter.StateMonad

    open Language
    open State

    type 'a stateMonad  
  
    val ret  : 'a -> 'a stateMonad  
    val fail : 'a stateMonad  
    
    val (>>=) : 'a stateMonad -> ('a -> 'b stateMonad) -> 'b stateMonad  
    val (>>>=) : 'a stateMonad -> 'b stateMonad -> 'b stateMonad
    
    val fail : 'a stateMonad

    // Green
    // val fail : stateContMonad<'a, 'r>

    val declare : string -> unit stateMonad
    val setVar : string -> int -> unit stateMonad
    val getVar : string -> int stateMonad
    val alloc : string -> int -> unit stateMonad
    val free : int -> int -> unit stateMonad
    val setMem : int -> int -> unit stateMonad
    val getMem: int -> int stateMonad
    val random : int stateMonad
    val evalState : state -> 'a stateMonad -> 'a option
    
    
    