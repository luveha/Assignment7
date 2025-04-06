module Interpreter.StateMonad

    open State

    type 'a stateMonad = SM of (state -> ('a * state) option)  
      
    let ret x = SM (fun st -> Some(x, st))  
    let fail  = SM (fun _ -> None)  
    
    let bind (SM f) g =  
        SM (fun st ->  
            match f st with  
            | Some (x, st') -> let (SM h) = g x in h st'  
            | None -> None)  
        
    let (>>=) a f = bind a f  
    let (>>>=) a b = a >>= (fun _ -> b)

    let declare (s: string) : unit stateMonad = 
        SM ( fun st ->
            match State.declare s st with
            | Some changedSt -> Some((),changedSt)
            | None -> None
        )

    let setVar  (s: string) (v: int): unit stateMonad  =
        SM ( fun st ->
            match State.setVar s v st with
            | Some changedSt -> Some((),changedSt)
            | None -> None
        )

    let getVar (s: string) : int stateMonad  = 
        SM ( fun st ->
            match State.getVar s st with
            | Some outInt -> Some(outInt, st)
            | None -> None
        )

    let alloc (s: string) (size: int): unit stateMonad = 
        SM ( fun st ->
            match State.alloc s size st with
            | Some changedSt -> Some((), changedSt)
            | None -> None
        )

    let free (ptr: int) (size: int): unit stateMonad =
        SM ( fun st ->
            match State.free ptr size st with
            | Some changedSt -> Some((), changedSt)
            | None -> None 
        ) 

    let setMem (ptr: int) (v: int): unit stateMonad = 
        SM ( fun st ->
            match State.setMem ptr v st with
            | Some changedSt -> Some((), changedSt)
            | None -> None
        )

    let getMem (ptr: int) : int stateMonad = 
        SM ( fun st ->
            match State.getMem ptr st with
            | Some outInt -> Some(outInt, st)
            | None -> None
        )

    let random : int stateMonad = SM (fun st -> Some(State.random st, st))

    let evalState (st: state) (SM func: 'a stateMonad) : 'a option =
        match func st with
        | Some (v,_) -> Some v
        | None -> None